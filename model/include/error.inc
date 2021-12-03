! Macros for error_manager.f

! Fortran is case insensitive, but C preprocessor is not, so call the macros like:
!
!   call LOG_ERROR()
!
!   or
!
!   call log_error()

! Do NOT call them as follows (would be compile-time error):
!
!   call log_error   ! no parentheses (could be supported by macros, but not worth the complexity)
!
!   or
!
!   call Log_Error   ! mixed case (not worth defining all 2**8 combinations of cases for LoG_eRrOr, etc.)

#define log_error() log_message(__FILE__,__LINE__, LOGLEVEL_ERROR, abort=.true.)
#define log_warning() log_message(__FILE__,__LINE__, LOGLEVEL_WARNING, abort=.false.)
#define log_status() log_message(__FILE__, __LINE__, LOGLEVEL_STATUS, header=.false., footer=.false., no_new_line=.false., abort=.false.)
#define log_info() log_message(__FILE__,__LINE__, LOGLEVEL_INFO, abort=.false.)

#define LOG_ERROR() log_error()
#define LOG_WARNING() log_warning()
#define LOG_STATUS() log_status()
#define LOG_INFO() log_info()
