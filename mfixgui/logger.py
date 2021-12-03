"""Methods for logging captured job output
See srs/output.rst"""

import os

class Logger(object):
    def init_logger(self):
        self.log_file_names = {} # File names
        self.log_files = {} # Handles to open files
        self.log_last = {} # Previous values
        self.log_line_buffer = '' # Partial line
        self.log_in_solid_inventory = False
        self.log_resid_strings = []
        self.log_solid_inventory_data = []

    def reset_logger(self):
        self.log_files.clear()
        self.log_last.clear()
        self.log_line_buffer = ''
        self.log_in_solid_inventory = False
        self.log_resid_strings = []
        self.log_solid_inventory_data = []

    def open_log_files(self):
        # Called before starting a job to set up capture files
        for (key, data) in self.output_logs.items():
            enable, fname, mode = data
            if enable and not fname:
                self.error("No log file name specified for %s, not saving" % key,
                           popup=True)
                enable = False
            if not enable:
                self.log_files[key] = None
                continue
            if not mode:
                mode = 'overwrite'
            ext = '.txt' if key=='solver_output' else '.csv'
            if not fname.lower().endswith(ext):
                fname += ext
            if os.path.exists(fname) and mode=='increment':
                fname = increment_file_name(fname)
            need_header = True
            if os.path.exists(fname) and os.stat(fname).st_size>0:
                need_header = False
            try:
                open_mode = 'at' if mode=='append' else 'wt'
                f = open(fname, mode=open_mode, buffering=1)
            except Exception as e:
                self.error("Cannot open file %s for %s: %s" %
                           (fname, key, e),
                           popup=True)
                f = None
            if f and need_header:
                try:
                    self.write_log_file_header(f, key)
                except Exception as e:
                    self.error("Error writing CSV header to %s: %s. Disabling log." % (fname, e),
                               popup=True)
            self.log_files[key] = f
            self.log_file_names[key] = fname


    def write_log_file_header(self, f, key):
        if key == 'solver_output': # Plain text file, no CSV header
            return
        elif key in ('dt', 'nit'):
            f.write('"time", "%s"\n'%key)
        elif key == 'residuals':
            self.log_resid_strings = [v for v in (self.project.get_value('resid_string', args=[i]) for i in range(1, 9))
                                      if v is not None]
            f.write(', '.join(['"time"'] + ['"%s"' for s in self.log_resid_strings]) + '\n')
        elif key == 'solid_inventory':
            if (self.project.get_value('breakdown_solid_inventory_by_phase', default=False)
                and len(self.solids) > 1):
                header = ['"time"'] + ['"%s"'%s for s in self.solids.keys()] + ['"total"']
            else:
                header = ['"time"', '"total"']
            f.write(', '.join(header) + '\n')
        else:
            raise ValueError(key)

    def close_log_files(self):
        for (key, val) in self.log_files.items():
            if val is not None:
                try:
                    val.close()
                except Exception as e:
                    self.log_files[key] = None
                    self.error("Error closing log file %s for %s: %s" %
                               (self.log_file_names.get(key, ''), key, e))


    def update_logger_solver_output(self, text, error=False):
        self.log_line_buffer += text
        lines = self.log_line_buffer.splitlines()
        if not lines:
            return
        # Partial line
        if not self.log_line_buffer.endswith('\n'):
            self.log_line_buffer = lines[-1]
            del lines[-1]
        else:
            self.log_line_buffer = ''

        for line in lines:
            if not line.strip(): # Skip blank lines
                continue
            if is_delimiter(line):
                continue
            f = self.log_files.get('solver_output')
            if f:
                try:
                    f.write(line+'\n')
                except Exception as e:
                    #Stop logging solver output for this run, to avoid further errors
                    self.log_files['solver_output'] = None
                    self.error("Error writing log file %s for solver output: %s" %
                               (self.log_file_names.get('solver_output', ''), e),
                               popup=True)

            # Only handle solid inventory if we are logging it
            # get_smass.f:1300  FORMAT('==========',1x,A,1x,'of solids inventory report ==========')

            if self.log_files.get('solid_inventory') is not None:
                lower = line.lower()
                if 'solids inventory report' in lower:
                    if 'start of' in lower:
                        self.log_in_solid_inventory = True
                        self.log_solid_inventory_data = []
                    elif 'end of' in lower:
                        self.log_in_solid_inventory = False
                        try:
                            self.update_logger_solid_inventory()
                        except Exception as e:
                            self.log_files['solid_inventory'] = None
                            self.error("Error writing log file %s for solid inventory: %s" %
                                       (self.log_file_names.get('solid_inventory', ''), e),
                                       popup=True)
                elif self.log_in_solid_inventory: # Do not log start/end lines
                    self.log_solid_inventory_data.append(line)


    def update_logger_solid_inventory(self):
        f = self.log_files.get('solid_inventory')
        if f is None:
            return
        # Solid inventory report looks like:
        #DEM solids mass at time =  0.00000E+00 sec:
        #For phase   1,   Sm_001 =  0.00000E+00 kg
        #For phase   2,   Sm_002 =  0.00000E+00 kg
        #       Total Sm =  0.00000E+00 kg
        vals = [float(line.split()[-2]) for line in self.log_solid_inventory_data]
        f.write(', '.join("%.6g"%v for v in vals) + '\n')
        self.log_solid_inventory_data = []


    def update_logger_status(self, status):
        # status: data dict returned from HTTP status query
        t = status.get('time', 0.0)
        for key in 'dt', 'nit', 'residuals':
            val = status.get(key)
            if val is None:
                continue

            f = self.log_files.get(key)
            if f is None: # no log file
                continue
            # Don't log if no change
            prev = self.log_last.get(key)
            self.log_last[key] = val
            if prev == val:
                continue
            try:
                #f.write("%.6g,%.6g\n" % (t, val))
                if key == 'residuals':
                    d = dict(val)
                    vals = [d.get(s, 0.0) for s in self.log_resid_strings]
                    sval = ', '.join(["%.6g" % v for v in vals])

                else:
                    sval = "%.6g" % val
                f.write("%.6g, %s\n" % (t, sval))
            except Exception as e:
                # Disable output to this log file for the rest of this run
                self.log_files[key] = None
                fname = self.log_file_names.get(key)
                self.error("Error writing log file %s for %s: %s. Disabling log." %
                           (fname, key, e),
                           popup=True)


def is_delimiter(line):
    return any(x in line for x in (
        '<<<<<===========================',
        '>>>>>---------------------------'))


def increment_file_name(fname):
    while os.path.exists(fname):
        base, ext = os.path.splitext(fname)
        if '_' in base:
            b1, b2 = base.rsplit('_', 1)
            if b2.isdigit():
                n = 1 + int(b2)
                fname = '%s_%s%s' % (b1, n, ext)
                continue
        fname = base + '_1' + ext
    return fname
