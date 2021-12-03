#!/usr/bin/env python

import argparse
import glob
import os
import signal
import socket
import sys
import tempfile
import ctypes

from threading import Thread

try:
    import mfixsolver as M
except ImportError as e:
    print("Cannot import solver module", e)
    raise

#Initialize libgfortran so we obtain stack traces
try:
    L = ctypes.cdll.LoadLibrary(M.__file__)
except OSError as e:
    print(e) # Should not happen, we already loaded M
    L = None
if L:
    try:
        set_options = L._gfortran_set_options
        set_fpe = L._gfortran_set_fpe
    except AttributeError as e:
        #print(e) # If not using gfortran, this is not an error
        set_options = None
    if set_options:
        OptsType = ctypes.c_int * 6
        opts = OptsType(68,   # warn_std - see compile_options.c in gfortran source.
                        511,  # allow_std
                        1,    # pedantic
                        1,    # backtrace
                        1,    # signed 0
                        63)   # run time checks
        sys.stdout.flush()
        set_options(6, opts)
        set_fpe(13)  # GFC_FPE_INVALID=1 | GFC_FPE_ZERO=4 | GFC_FPE_OVERFLOW=8
                     # catches sqrt(-1), 1/0 and overflows

port = 0 # OS finds free port
job_id = None
job_is_remote = False

run_cmd = os.environ.get('MFIX_RUN_CMD', ' '.join(sys.argv))

run_name = None # gets set when *.mfix file is loaded

class MfixThread(Thread):
    def __init__(self, filename, loglevel):
        Thread.__init__(self)
        self.filename = filename
        self.loglevel = loglevel

    def run(self):
        return M.main.run_mfix0(bytes(self.filename, encoding="utf-8"), self.loglevel)


def parse_args():
    parser = argparse.ArgumentParser(description='Welcome to PYMFiX')
    ARG = parser.add_argument
    ARG("-f", "--file",
        default="mfix.dat",
        help="specify an input file (*.mfx or *.dat)")

    ARG("-p", "--print-flags",
        action="store_true",
        help="display the solver configuration flags")

    ARG("-l", "--log",
        metavar="LOG",
        action="store",
        default="INFO",
        choices=["ERROR", "WARNING", "STATUS", "INFO"],
        help="set logging level (ERROR, WARNING, STATUS, INFO)")

    ARG("-c", "--clean",
        action="store_true",
        help="clean output files before starting run")

    ARG("-s", "--server", action="store_true", help="start HTTP server")
    return parser.parse_args()


def clean():
    """Remove output files from previous solver run. Must be called AFTER
    parallel_init() so that mype is initialized.
    """
    if M.compar.mype != 0:
        return
    for ext in 'vt?', 'sp?', 'out', 'res', 'pvd', 'res', 'log', 'pid', 'env', 'msh':
        for ext in ext, ext.upper():
            for f in glob.glob('*.' + ext):
                print("removing %s" % f)
                os.remove(os.path.normcase(os.path.realpath(f)))
    for f in glob.glob('fort.*'):
        os.unlink(f)


def get_run_name(filename):
    global run_name
    for line in open(filename, encoding='utf-8', errors='replace'):
        line = line.strip()
        if line.lower().startswith('run_name') and '=' in line:
            tok = line.split('=', 1)[1].strip()
            # Remove quotes if present.
            # NB: Due to a bug, in some files, run_name may lack quotes
            if tok.startswith('"') or tok.startswith("'"):
                tok = tok[1:]
            if tok.endswith('"') or tok.endswith("'"):
                tok = tok[:-1]
            run_name = tok.strip()
            break


ssl_enabled = False
def setup_ssl():
    global ssl_enabled
    ssl_enabled = False


def write_pidfile():
    # Avoid importing Flask in global namespace, to keep -p etc fast
    from .pymfix_webserver import secret_key

    pid = os.getpid()
    protocol = 'https' if ssl_enabled else 'http'

    if job_is_remote:
        hostname = socket.gethostname()
    else:
        # issues/407 just use loopback for local jobs
        hostname = '127.0.0.1'
    with open(run_name + '.pid', 'w', encoding='utf-8', errors='replace') as f:
        f.write('host=%s\n' % hostname)
        f.write('pid=%s\n' % pid)
        f.write('url=%s://%s:%s\n' %
                (protocol, hostname, port))
        f.write('token=x-pymfix-auth:%s\n' % secret_key)
        f.write('job_id=%s\n' % job_id)
        f.write('run_cmd=%s\n' % run_cmd)
    # Save environment for debugging
    with open(run_name+'.env', 'w', encoding='utf-8', errors='replace') as f:
        for (k,v) in sorted(os.environ.items()):
            f.write('%s=%s\n' % (k,v))


def find_free_port():
    # Note that this is racy - the port could get used by
    # another program before the webserver binds to it
    global port
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.bind(("", 0))
    port = sock.getsockname()[1]
    sock.close()


def redirect_stdio():
    # Send stdout/stderr to tempfiles so we can collect them and send to
    # client via HTTP
    pid = os.getpid()
    stdout_file = tempfile.TemporaryFile(prefix='mfix-%s-out-' % pid, buffering=0)
    stderr_file = tempfile.TemporaryFile(prefix='mfix-%s-err-' % pid, buffering=0)
    os.dup2(stdout_file.fileno(), sys.stdout.fileno())
    os.dup2(stderr_file.fileno(), sys.stderr.fileno())


def start_webserver():
    # Avoid importing pymfix_webserver in global namespace,
    # because we don't always need it and loading Flask is slow
    from . import pymfix_webserver as W

    # Set globals in W
    W.M = M
    W.mfix_thread = mfix_thread
    W.run_name = run_name
    W.job_is_remote = job_is_remote
    W.start(port=port)


def main():
    global mfix_thread, options, job_id, job_is_remote
    options = parse_args()
    job_id = (
        os.environ.get("JOB_ID")  # GridEngine
        or os.environ.get("PBS_JOBID")  # Torque
        or os.environ.get("SLURM_JOB_ID")  # SLURM
        or None  # local
    )
    job_is_remote = (job_id is not None) or ("qsub" in run_cmd) or ("sbatch" in run_cmd)

    # exit with Ctrl-C at the terminal
    signal.signal(signal.SIGINT, signal.SIG_DFL)

    if options.print_flags:
        M.main.print_flags()
        sys.exit(0)

    dirname, filename = os.path.split(options.file)
    if dirname:
        os.chdir(dirname)

    loglevels = {"ERROR": 1, "WARNING": 2, "STATUS": 3, "INFO": 4}
    loglevel = loglevels.get(options.log, 3)

    # set the global run_name variable
    # We do this here instead of getting it from the solver,
    #  because the solver may fail to load the input file
    get_run_name(filename)
    if not run_name:
        print("ERROR, run_name not set in %s" % filename)
        sys.exit(1)

    # TODO check if pidfile exists & process is running.  If so, refuse
    #  to start

    M.parallel_mpi.parallel_init()
    os.environ["FLASK_ENV"] = "development"

    if options.clean and M.compar.mype == 0:
        clean()

    if options.server:
        mfix_thread = MfixThread(filename, loglevel)
        mfix_thread.setDaemon(True) #Note, this is not controlled by -d flag, mfix_thread is always daemonic

        mfix_thread.start()

        # start the Flask server on rank 0
        if M.compar.mype == 0:
            if job_is_remote:
                redirect_stdio() # Should we do this on all nodes?
            #setup_ssl() #Does nothing at present
            find_free_port()
            write_pidfile()
            start_webserver()
            try:
                os.unlink(run_name + '.pid')
            except:
                pass
        else:
            mfix_thread.join()

    else: # --server not specified, don't start webserver
        # don't need separate thread for mfix
        M.main.run_mfix0(filename, loglevel)

    M.parallel_mpi.parallel_fin()


if __name__ == '__main__':
    main()
