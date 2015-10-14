 !     ******************************************************************
!     ablparms.f90
!     Author: Jon D. Richards    U.S. Geological Survey
!     Last change: JDR 2/21/2007 9:01:36 AM
!     ******************************************************************                                                          
!=============================================================================!

MODULE ablparms



!     MODULE ablparms
! DESCRIPTION:
!     Holds some high-level Blossom parameters.
! LANGUAGE:
!     Fortran 90/95
! AUTHOR:
!     Jon D. Richards, Operations Research Analyst       jon_richards@usgs.gov
!     Fort Collins Science Center                     http://www.fort.usgs.gov
!     U.S. Geological Survey          -          Biological Resources Division
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
IMPLICIT NONE
SAVE

! new regression testing toggle
! LOGICAL :: l_tn

INTEGER :: CurrentPID ! Current Process ID in Windows

LOGICAL, PARAMETER :: l_WINDOWS_VERSION = .FALSE.                              !#/CON/
!#/WINTERACTER/ LOGICAL, PARAMETER :: l_WINDOWS_VERSION = .TRUE.                               !#/WINTERACTER/
CHARACTER (LEN=*), PARAMETER :: ch_BLOSSOM_PROMPT = ">"
! Maximum Size of File or Path or FileSpec or PathSpec
INTEGER, PARAMETER :: MAX_PATH                  = 260 ! from cheaderinfo - jdr
        ! Maximum size for DOS COMSPEC
INTEGER, PARAMETER :: i_MAX_LEN_DOS_COMSPEC     = 64

        ! Maximum DOS path length
INTEGER, PARAMETER :: i_MAX_LEN_DOS_PATH        = 1024
        !   Maximum number of elements that can be in a Blossom
        !   command.
INTEGER, PARAMETER :: i_MAX_DEPTH               = 1536
        ! Static for now- JDR 4 Feb 1999
        !   Maximum number of variables that can be in a data
        !   file USEd by Blossom (only numeric variables are USEd).
INTEGER, PARAMETER :: i_MAX_N_VARS              = 1024
        !   INTEGER, PARAMETER :: i_MAX_N_VARS              = 512
        ! Maximum number of Observations (limit of virtual memory):
INTEGER, PARAMETER :: i_MAX_N_OBS               = HUGE(1)
        ! Maximum number of LAD independent variables. (i_MAX_N_VARS or ??)
INTEGER, PARAMETER :: i_MAX_LAD_VARS            = 65536
        ! Maximum number of elements in the USEd data input buffer.
INTEGER, PARAMETER :: i_MAX_NUM_USE_ELMS        = i_MAX_N_VARS
        ! Maximum number of MRSP variables.
! INTEGER, PARAMETER :: i_MAX_SP_VARS             = i_MAX_N_VARS - 1
INTEGER, PARAMETER :: i_MAX_SP_VARS             = 65536
        ! Maximum number multi-response (MRPP,MRBP,...) variables
! INTEGER, PARAMETER :: i_MAX_MR_VARS             = i_MAX_N_VARS - 1
INTEGER, PARAMETER :: i_MAX_MR_VARS             = 65536
        ! MS-DOS filename size limit
! INTEGER, PARAMETER :: i_DOS_FN_SIZE             = 25
INTEGER, PARAMETER :: i_DOS_FN_SIZE             = MAX_PATH ! Apr 2006 - jdr
! INTEGER, PARAMETER :: i_DOS_FN_SIZE             = 12
        ! Maximum Blossom command element size.
INTEGER, PARAMETER :: i_MAX_ELEM_LEN            = 25
        ! Maximum length Blossom variable names.
INTEGER, PARAMETER :: i_MAX_LEN_VAR_NAME        = i_MAX_ELEM_LEN
        ! Maximum length of filespec (filename).
        ! We want to limit user to enter only the save file name, which always gets saved
        ! to the user's My Blossom folder.
INTEGER, PARAMETER :: i_MAX_LEN_SAVEFILE_SPEC       = i_MAX_ELEM_LEN
! SYSTAT stuff:
        ! Maximum length of Systat character variable.
INTEGER, PARAMETER :: i_MAX_LEN_SYSTAT_CHAR_VAR = 12
        ! Maximum length of Systat character variable name record length.
INTEGER, PARAMETER :: i_SIZE_SYSTAT_CH_VAR_REC  = i_MAX_LEN_SYSTAT_CHAR_VAR
        ! Systat physical block size.
INTEGER, PARAMETER :: i_SYSTAT_PHYS_BLOCK_SIZE  = 128
        ! Systat mark that logical record is continued across physical
        !      block boundaries.
INTEGER, PARAMETER :: i_SYSTAT_CONTINUED_BLOCK  = 129
        ! Systat mark that there is no more data (may or may not exist).
INTEGER, PARAMETER :: i_SYSTAT_TERMINATOR_MARK  = 130
        ! Maximum number of REAL (KIND(0.0)) values that fit in Systat
        !      physical block.
INTEGER, PARAMETER :: i_MAX_SYSTAT_R4_VALS_PER_REC = 32
        ! Maximum number of REAL (KIND(0D0)) values that fit in Systat
        !      physical block.
INTEGER, PARAMETER :: i_MAX_SYSTAT_R8_VALS_PER_REC = 16
        ! Indicates Systat data are single precision: type REAL (KIND(0.0))
INTEGER, PARAMETER :: i_SYSTAT_SINGLE_PREC      = 1
        ! Indicates Systat data are double precision: type REAL (KIND(0D0))
INTEGER, PARAMETER :: i_SYSTAT_DOUBLE_PREC      = 2
        ! Where to start reading Systat file: at record 2 (2nd byte).
INTEGER, PARAMETER :: i_FIRST_SYSTAT_RECORD     = 2
        ! Systat file type.
INTEGER, PARAMETER :: i_SYSTAT_RECTANGULAR      = 1
        ! Systat file type.
INTEGER, PARAMETER :: i_SYSTAT_SSCP             = 2
        ! Systat file type.
INTEGER, PARAMETER :: i_SYSTAT_COVARIANCE       = 3
        ! Systat file type.
INTEGER, PARAMETER :: i_SYSTAT_CORRELATION      = 4
        ! Systat file type.
INTEGER, PARAMETER :: i_SYSTAT_DISSIMILARITY    = 5
        ! Systat file type.
INTEGER, PARAMETER :: i_SYSTAT_SIMILARITY       = 6
        ! First byte value in Systat data set.
INTEGER, PARAMETER :: i_SYSTAT_FIRST_BYTE_VALUE = 75
        ! First Systat logical record length.
INTEGER, PARAMETER :: i_SYSTAT_FIRST_LOG_RECL   = 6
! S-Plus stuff:
        ! Information to check for S-Plus data frame file.
INTEGER, PARAMETER :: i_ADDR_SD_CHK             = 2
INTEGER, PARAMETER :: i_LEN_SD_CHK              = 6
CHARACTER (LEN=i_LEN_SD_CHK), PARAMETER :: ch_SDataCheck = "S data"
        ! Where in an S-Plus data frame file the
        !   number of cases can be found.
INTEGER, PARAMETER :: i_ADDR_NCAS               = 59
        ! Where in an S-Plus data frame file the
        !   number of bytes in a variable name block can be found.
INTEGER, PARAMETER :: i_ADDR_NUM_BY_VN          = 79
        ! Where in an S-Plus data frame file the
        !   number of variables can be found.
INTEGER, PARAMETER :: i_ADDR_NVAR               = 55
        ! Where in an S-Plus data frame file the
        !   variable names block can be found.
INTEGER, PARAMETER :: i_ADDR_VAR_NAMES          = 83
        ! Default Blossom Input Buffer size.
INTEGER, PARAMETER :: i_IN_BUFFER_SIZE          = 80 ! ???? Can we make this larger - jdr 01/18/2005
        ! Maximum length of SUBMIT filespec. This may be a relative or
        !      absolute address.
! INTEGER, PARAMETER :: i_MAX_LEN_SUB_SPEC        = 80 ! ???? Can we make this larger - jdr 01/18/2005
INTEGER, PARAMETER :: i_MAX_LEN_SUB_SPEC        = 260 ! Apr 2006 - jdr
!         ! Maximum length of path for change directory command.
! INTEGER, PARAMETER :: i_MAX_PATH_LEN            = 80 ! ???? Can we make this larger - jdr 01/18/2005
        ! Maximum length for output title.
INTEGER, PARAMETER :: i_MAX_TITLE_LEN           = 80
        ! double precision output width allowed when written as text.
INTEGER, PARAMETER :: i_DP_OUT_WIDTH            = 25
        ! Integer output width allowed when written as text.
INTEGER, PARAMETER :: i_INT_OUT_WIDTH           = 12
        ! Byte size of double precision: REAL (KIND(0D0))
INTEGER, PARAMETER :: i_SIZE_OF_REAL8           = 8
        ! Byte size of single precision: REAL (KIND(0.0))
INTEGER, PARAMETER :: i_SIZE_OF_REAL4           = 4
        ! Byte size of INTEGER (KIND=2)
INTEGER, PARAMETER :: i_SIZE_OF_INTEGER2        = 2
        ! Byte size of INTEGER (KIND=4)
INTEGER, PARAMETER :: i_SIZE_OF_INTEGER4        = 4
        ! Size of short (unique) Blossom command.
INTEGER, PARAMETER :: i_LEN_UNIQUE_COMMAND      = 2
        ! Number of Blossom commands to save (current and any previous).
INTEGER, PARAMETER :: i_NUM_CMDS_SAVED          = 2
        ! Current command in Blossom session.
INTEGER, PARAMETER :: i_THIS_CMD                = 1
        ! Previous command in Blossom session.
INTEGER, PARAMETER :: i_PREVIOUS_CMD            = 2
        ! Number of LAD commands to save (current and any previous).
INTEGER, PARAMETER :: i_NUM_LAD_CMDS            = 2
        ! Default number of permutations for LAD tests.
INTEGER, PARAMETER :: i_DEFAULT_NUM_LAD_PERMS   = 5000
        ! Default number of resamples for MC est of P-Value of MRPP.
INTEGER, PARAMETER :: i_DEFAULT_NUM_MRPP_PERMS  = 5000
        ! Default number of permutations for COVerage tests.
REAL (KIND(0D0)), PARAMETER :: d_DEFAULT_LAD_THETA = -0.5D0
        ! Maximum size of Blossom command.  Limited for interactive to
        !      current keystroke buffer size, but for SUBMIT files we can
        !      have larger commands, up to this size.
! INTEGER, PARAMETER :: i_MAX_CMD_LENGTH          = 4096
INTEGER, PARAMETER :: i_MAX_CMD_LENGTH          = 8192
        ! Code indicating this USE file is Blossom ASCII text data.
INTEGER, PARAMETER :: i_BLOSSOM_FILE  = 0
        ! Code indicating this USE file is in DIF text data format.
INTEGER, PARAMETER :: i_DIF_FILE      = 1
        ! Code indicating this USE file is a Systat data format.
INTEGER, PARAMETER :: i_SYSTAT_FILE   = 2
        ! Maximum OUTPUT file record size.
! INTEGER, PARAMETER :: i_MAX_OUT_RECL = 140
INTEGER, PARAMETER :: i_MAX_OUT_RECL = i_MAX_CMD_LENGTH
INTEGER, PARAMETER :: i_CHKSUM = 5183
INTEGER, PARAMETER :: i_SIM_UNIT = 89
END MODULE ablparms



!     ******************************************************************
!     blcmnmod.f90
!     Author: Jon D. Richards    U.S. Geological Survey
!     Last change: JDR 9/21/2007 11:31:17 AM
!     ******************************************************************

MODULE blcmnmod
!___Imported Parameters and Variables:
USE ablparms, ONLY: i_MAX_DEPTH, i_MAX_N_VARS, i_MAX_LEN_VAR_NAME,            &
      i_MAX_ELEM_LEN, i_MAX_LEN_DOS_COMSPEC, i_MAX_LEN_DOS_PATH,              &
      i_MAX_TITLE_LEN
        ! ^-- Holds some high-level Blossom parameters.
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     MODULE blcmnmod
! DESCRIPTION:
!     Some global Blossom variables. - JDR  15 Jan 1999
!     Derived from old Blossom block data common blocks.
! LANGUAGE:
!     Fortran 90/95
! AUTHOR:
!     Jon D. Richards, Operations Research Analyst       jon_richards@usgs.gov
!     Fort Collins Science Center                     http://www.fort.usgs.gov
!     U.S. Geological Survey          -          Biological Resources Division
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
IMPLICIT NONE
SAVE
REAL (KIND(0D0)), PARAMETER :: d_PI = 3.14159265358979323846264338327950288419716939937510D0
INTEGER :: i_WinPos
LOGICAL :: l_Console       ! blossom errors to console? (or false=>dialog box).
LOGICAL :: l_LogFileOn     !
LOGICAL :: l_EchoData      ! whether to echo USE file data to screen when read.
LOGICAL :: l_EchoOutput    ! does user want results screen output? (default=on)
LOGICAL :: l_HasOutFile    ! does Blossom currently have an OUTPUT file?
LOGICAL :: l_HasSaveFile   ! does Blossom currently have a SAVE file?
LOGICAL :: l_HasTitle      ! does Blossom currently have a Title for outputs?
LOGICAL :: l_HasUseFile    ! does Blossom currently have a USE file?
LOGICAL :: l_IsSwapped     ! are essential data still only in the SWAP file?
LOGICAL :: l_LabelFile     ! is current ASCII text Blossom DAT file labeled?
LOGICAL :: l_OutputCmd     ! did user specify an explicit OUTPUT filename?
LOGICAL :: l_Terse         ! Terse output?
! the user command that has been broken down into elements.
! these elements were separated by spaces, commas, hyphens,
! parentheses, and such. all separators are removed and the
! elements themselves are stored here.
CHARACTER (LEN=i_MAX_ELEM_LEN) :: cha_CmdStack(i_MAX_DEPTH)
! the list of variable names for the current TEMP data set.
! these correspond to the USE variable name list, except that
! Blossom doesn't use character variables.
CHARACTER (LEN=i_MAX_LEN_VAR_NAME) :: cha_VarList(i_MAX_N_VARS)
! array of 1=inn,0=out that tells which variables in cha_VarList
! are active in the current command.
INTEGER :: ia_ActiveVarNdx (i_MAX_N_VARS)
! array giving positions in the cha_VarList array of the variables
! in the current command.  if cha_VarList has (V1, V2, V3) then
! a command that uses variables V3 and V1 in that order will result
! in this array having values (3, 1)
INTEGER :: ia_CmdVarNdx(i_MAX_N_VARS)
INTEGER :: i_NumCases   ! number of cases (observations) in current data
INTEGER :: i_NumVars    ! number of variables in current TEMP data set
INTEGER :: i_StackDepth ! number of elements in cha_CmdStack
INTEGER :: i_StackPosn  ! current parsing position in cha_CmdStack
        
INTEGER, PARAMETER :: i_NO_FILE_CONNECTED_TO_UNIT = 138
INTEGER, PARAMETER :: i_ZERO = 0
INTEGER, PARAMETER :: i_ONE  = 1
INTEGER, PARAMETER :: i_ON   = 1
INTEGER, PARAMETER :: i_OFF  = 0
REAL (KIND(0D0)), PARAMETER :: d_ZERO = 0.0D0
REAL (KIND(0D0)), PARAMETER :: d_ONE  = 1.0D0
REAL (KIND(0D0)), PARAMETER :: d_HALF = 0.5D0
REAL (KIND(0D0)), PARAMETER :: d_TWO  = 2.0D0
LOGICAL, PARAMETER :: l_TRUE  = .TRUE.
LOGICAL, PARAMETER :: l_FALSE = .FALSE.
LOGICAL, PARAMETER :: l_CLEAR = .TRUE.
INTEGER, PARAMETER :: i_ARRAY_NOT_POS_DEF =  -8 ! Array not positive definite
INTEGER, PARAMETER :: i_ARRAY_POS_DEF     =  -7 ! Array tested pos definite
INTEGER, PARAMETER :: i_BAD_DIF_FILE      = -29
INTEGER, PARAMETER :: i_BAD_SPLUS_FILE    = -28
INTEGER, PARAMETER :: i_BAD_SYSTAT_FILE   = -22
INTEGER, PARAMETER :: i_BAIL_OUT          =  -5 ! bail out of an analysis
INTEGER, PARAMETER :: i_BOUNDS_ERROR      =  -6 ! Array address out of bounds
INTEGER, PARAMETER :: i_DEFWIND_ERROR     = -34 ! define window error: tselect
INTEGER, PARAMETER :: i_DIV_BY_ZERO       = -35 ! An attempt made to / 0
INTEGER, PARAMETER :: i_DOS_ERR_EXIT      =   1 ! Exit to DOS with error.
INTEGER, PARAMETER :: i_END_SYSTAT_FILE   = -23
INTEGER, PARAMETER :: i_NOTE_DONE         =  -2 ! note was written to LOG file
INTEGER, PARAMETER :: i_NOT_OK            =  -1 ! Error status not OK.
INTEGER, PARAMETER :: i_NO_FILE_MATCH     = -33 ! from findfirst
INTEGER, PARAMETER :: i_OK                =   0 ! Error status OK.
INTEGER, PARAMETER :: i_VAR_LST_ERROR     = -42 ! Hypothesis var list error
INTEGER, PARAMETER :: i_OUT_UNIT      = 32 ! File Unit
        ! Lahey Fortran Error Code for "End of Direct Access" file reached.
INTEGER, PARAMETER :: i_END_DIRECT_ACCESS = 80
INTEGER, PARAMETER :: i_ZERO_LENGTH_FILE_DELETED = 1
INTEGER, PARAMETER :: i_UNIT_FILENAME_NOT_MATCH  = 2
INTEGER, PARAMETER :: i_FILENAME_NOT_SPECIFIED   = 3
INTEGER, PARAMETER :: i_ERROR_OPEN_SEQ_FILE      = 4
INTEGER, PARAMETER :: i_FILE_NOT_SEQUENTIAL      = 5  ! Not zero-length then
INTEGER, PARAMETER :: i_FILE_DID_NOT_EXIST       = 6
INTEGER, PARAMETER :: I_FILE_NOT_ZERO_LENGTH     = 7
INTEGER, PARAMETER :: i_UNKNOWN_PROBLEM          = 8
INTEGER, PARAMETER :: i_EOF_EOR                  = -1
INTEGER, PARAMETER :: i_OUT_OF_RANGE             = -200
! user's output title, used to "head" sections of results in OUTPUT file.
CHARACTER (LEN=i_MAX_TITLE_LEN)       :: ch_Output_Title
! for DOS, the address to the COMMAND.COM file. (NT ? to CMD.EXE ?)
CHARACTER (LEN=i_MAX_LEN_DOS_COMSPEC) :: ch_DOS_ComSpec
! current path to present position in file system.
CHARACTER (LEN=i_MAX_LEN_DOS_PATH)    :: ch_DOS_Path
! "Break" line for output file.  Goes between outputs from stat procedures.
CHARACTER (LEN=*), PARAMETER :: ch_BREAK_LINE_FORMAT = "(/,1X,70('='),/)"
CHARACTER (LEN=*), PARAMETER :: ch_NL    = CHAR(13)//CHAR(10)
CHARACTER (LEN=*), PARAMETER :: ch_EMPTY = ""

CHARACTER (LEN=*), PARAMETER :: ch_BACKSLASH = ACHAR(92)  !  "\"

CHARACTER (LEN=*), PARAMETER :: ch_COLON     = ACHAR(58) ! ":"
CHARACTER (LEN=*), PARAMETER :: ch_BLANK     = ACHAR(32) ! " "
CHARACTER (LEN=*), PARAMETER :: ch_SLASH     = ACHAR(47) ! "/"
CHARACTER (LEN=*), PARAMETER :: ch_ASTER     = ACHAR(42) ! "*"
CHARACTER (LEN=*), PARAMETER :: ch_EQUAL     = ACHAR(61) ! "="
CHARACTER (LEN=*), PARAMETER :: ch_HYPHEN    = ACHAR(45) ! "-"
CHARACTER (LEN=*), PARAMETER :: ch_PLUS      = ACHAR(43) ! "+"
CHARACTER (LEN=*), parameter :: ch_UNDSCORE  = ACHAR(95) ! "_"
CHARACTER (LEN=*), PARAMETER :: ch_SPEC_CHARS =          &
      ch_BLANK//ch_SLASH//ch_ASTER//ch_EQUAL//ch_HYPHEN//ch_PLUS
CHARACTER (LEN=*), PARAMETER :: ch_DOT       = ACHAR(46) ! "."
CHARACTER (LEN=*), PARAMETER :: ch_Q_MARK    = ACHAR(63) ! "?"
CHARACTER (LEN=*), PARAMETER :: ch_WILD_CHARS   = ch_ASTER//ch_Q_MARK
CHARACTER (LEN=*), PARAMETER :: ch_UPPER_ALPHABET =      &
                                          "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
CHARACTER (LEN=*), PARAMETER :: ch_LOWER_ALPHABET =      &
                                          "abcdefghijklmnopqrstuvwxyz"
CHARACTER (LEN=*), PARAMETER :: ch_NUMBERS      = "0123456789"
CHARACTER (LEN=*), PARAMETER :: ch_OK_NUMB_CHARS =       &
      ch_NUMBERS//ch_DOT//ch_HYPHEN
CHARACTER (LEN=*), PARAMETER :: ch_OK_NAME_CHARS =       &
      ch_UPPER_ALPHABET//ch_NUMBERS//ch_UNDSCORE
CHARACTER (LEN=*), PARAMETER :: ch_QUOTE   = ACHAR(34) ! '"'
CHARACTER (LEN=*), PARAMETER :: ch_APOST   = ACHAR(39) ! "'"
CHARACTER (LEN=*), PARAMETER :: ch_NULL    = ACHAR(0)
CHARACTER (LEN=*), PARAMETER :: ch_TAB     = ACHAR(9)
CHARACTER (LEN=*), PARAMETER :: ch_COMMA   = ACHAR(44) ! ","
CHARACTER (LEN=*), PARAMETER :: ch_L_PAREN = ACHAR(40) ! "("
CHARACTER (LEN=*), PARAMETER :: ch_R_PAREN = ACHAR(41) ! ")"
CHARACTER (LEN=*), PARAMETER :: ch_EOF_MARKER = ACHAR(26) ! Ctrl-Z
CHARACTER (LEN=*), PARAMETER :: ch_CHANGE_CHARS =        &
      ch_COMMA//ch_L_PAREN//ch_R_PAREN//ch_NULL//ch_TAB
CHARACTER (LEN=*), PARAMETER :: ch_END_OF_STACK = "<END>"
CHARACTER (LEN=40), PARAMETER :: ch40_BLANKS =            &
     "                                        "
CHARACTER (LEN=*), PARAMETER :: ch_DIR_SEP = ch_BACKSLASH  ! "\"; directory separator
! current working directory:
CHARACTER (LEN=*), PARAMETER :: ch_CWD = ch_DOT//ch_DIR_SEP
!
! _ini_
! _ini_ ! Blossom.ini parameters:
! Use as key value in registry entry:
CHARACTER (LEN=*), PARAMETER :: ch_INI_S_STRING = "LastDataPath"
! _ini_ We eliminate the BLOSSOM.INI File - JDR Mar 2006
! _ini_ CHARACTER (LEN=*), PARAMETER ::  ch_INI_SECTION = "[Blossom WIN]"
!
! Blossom Registry subkey and values
CHARACTER (LEN=*), PARAMETER :: BLOSSOM_SUBKEY = "SOFTWARE\USGS\Blossom"
CHARACTER (LEN=*), PARAMETER :: BLOSDIR_VALUE_NAME = "BLOSSDIR"
!
! C Language parameters:
!  I made up this parameter name to match C language return value for BOOL false. - JDR
INTEGER, PARAMETER :: C_false = 0  ! C language convention: any nonzero value is true
!
! a "logo" to print when Blossom is launched.
CHARACTER (LEN=*), PARAMETER ::  ch_VERSION_NUM  = "2007.09.21"
CHARACTER (LEN=*), PARAMETER ::  ch_BLOSSOM_LOGO = &
!#/WINTERACTER/ "      _$n    _(_)_$n   (_)@(_)$n     (_)$n  ._ |$n   \)| _.$n    \|(/$n     |/$n"//&!#/WINTERACTER/
"     _$n    _(_)_$n   (_)@(_)$n     (_)$n  ._ |$n   \)| _.$n    \|(/$n     |/$n"//&!#/CON/
!#/WINTERACTER/ ".--- --- -.$nBlossom Version W"//ch_VERSION_NUM//"$n"//&                      !#/WINTERACTER/
".--- --- -.$nBlossom Version C"//ch_VERSION_NUM//"$n"//&                      !#/CON/
"Fort Collins Science Center           $n"//&
"U.S. Geological Survey$n2150 Centre AV BLDG C$nFort Collins, CO 80526-8118, USA"//&
"$nhttp://www.fort.usgs.gov/products/software/blossom/blossom.asp"


INTEGER, PARAMETER :: i_LEN_VN_BUFF = 65536
CHARACTER (LEN=i_LEN_VN_BUFF) :: ch_VNBuff  ! buffer of variable names in s+ data frame
END MODULE blcmnmod


MODULE ioformod
!___Imported Parameters and Variables:
USE ablparms, ONLY: i_DP_OUT_WIDTH, i_INT_OUT_WIDTH
        ! ^-- Some high-level Blossom parameters.
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     MODULE ioformod
! DESCRIPTION:
!     ioformod.f90 -  change formats of numbers for I/O.
! LANGUAGE:
!     Fortran 90/95
! AUTHOR:
!     Jon D. Richards, Operations Research Analyst jon_richards@usgs.gov
!     Fort Collins Science Center               http://www.fort.usgs.gov
!     U.S. Geological Survey          -          Biological Resources Division
! MODIFICATION HISTORY:
!     DATE CREATED:   28 Jun 1999
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
IMPLICIT NONE
SAVE
CHARACTER (LEN=i_INT_OUT_WIDTH) :: ch12_OutBuff, ch12_OutBuff2,               &
      ch12_OutBuff3, ch12_OutBuff4, ch12_OutBuff5, ch12_OutBuff6,             &
      ch12_OutBuff7, ch12_OutBuff8
CHARACTER (LEN=i_DP_OUT_WIDTH) :: ch25_OutBuff, ch25_OutBuff2, ch25_OutBuff3, &
      ch25_OutBuff4
PUBLIC ch12_OutBuff, ch12_OutBuff2, ch12_OutBuff3, ch12_OutBuff4,             &
      ch12_OutBuff5, ch12_OutBuff6, ch12_OutBuff7, ch12_OutBuff8,             &
      ch25_OutBuff, ch25_OutBuff2, ch25_OutBuff3, ch25_OutBuff4
CONTAINS

!=============================================================================!

SUBROUTINE getint4(ch_Buff, &
                   i_Numb,  &
                   iEr)
!___Imported Parameters and Variables:
USE blcmnmod, ONLY: i_OK, i_NOT_OK
        ! ^-- Some global Blossom variables and parameters.
!___Imported Procedures:
!__(none)__
IMPLICIT NONE
!___Dummy Arguments:
CHARACTER (LEN=*), INTENT(IN)  :: ch_Buff
INTEGER,           INTENT(OUT) :: i_Numb
INTEGER,           INTENT(OUT) :: iEr
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     SUBROUTINE getint4
! DESCRIPTION:
!     Get an INTEGER*4 number from ch_Buff and put into i_Numb.
!     If error, return 0 in i_Numb and error code.

!___Local Variables:
INTEGER :: ios

iEr = i_OK ! no problem yet
READ(UNIT=ch_Buff,    &
      FMT='(BN,I25)', &
   IOSTAT=ios)        &
         i_Numb
IF (ios /= i_OK) THEN
   i_Numb = 0
   iEr   = i_NOT_OK
   RETURN   ! ... error exit ...
END IF
iEr = i_OK
RETURN   ! ... normal exit ...
END SUBROUTINE getint4

!=============================================================================!

END MODULE ioformod

MODULE simmod

! AUTHOR:
!     Jon D. Richards, Operations Research Analyst       jon_richards@usgs.gov
!     Fort Collins Science Center                     http://www.fort.usgs.gov
!     U.S. Geological Survey          -          Biological Resources Division
IMPLICIT NONE
SAVE
!   LOGICAL, PARAMETER :: lo_IS_SIMUL = .TRUE.
LOGICAL, PARAMETER :: lo_IS_SIMUL = .FALSE.
END MODULE simmod


MODULE csgamma
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     MODULE csgamma
! DESCRIPTION:
!     Chi-square and gamma function computations from Alan Miller's code.

IMPLICIT NONE
SAVE
CONTAINS

!=============================================================================!

FUNCTION lngamma(d_Z) RESULT(d_Lanczos)
IMPLICIT NONE
!___Dummy Arguments:
REAL (KIND(0D0)), INTENT(IN) :: d_Z
REAL (KIND(0D0)) :: d_Lanczos
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     FUNCTION lngamma
! DESCRIPTION:
!     Uses Lanczos-type approximation to ln(gamma) for d_Z > 0.
!     Reference:

REAL (KIND(0D0)), PARAMETER :: da_A(9) = &
      (/      0.9999999999995183D0,      676.5203681218835D0,      &
          -1259.139216722289D0,          771.3234287757674D0,      &
           -176.6150291498386D0,          12.50734324009056D0,     &
             -0.1385710331296526D0,        0.9934937113930748D-05, &
              0.1659470187408462D-06 /)
REAL (KIND(0D0)), PARAMETER :: d_ZERO      = 0.0D0
REAL (KIND(0D0)), PARAMETER :: d_ONE       = 1.0D0
REAL (KIND(0D0)), PARAMETER :: d_HALF      = 0.5D0
REAL (KIND(0D0)), PARAMETER :: d_LNSQRT2PI = 0.9189385332046727D0
REAL (KIND(0D0)), PARAMETER :: d_SIXPT5    = 6.5D0
REAL (KIND(0D0)), PARAMETER :: d_SEVEN     = 7.0D0
!___Local Variables:
REAL (KIND(0D0))  :: d_Tmp
INTEGER       :: j
!___Intrinsic Procedures:
INTRINSIC :: LOG
!___Executable Statements:
IF (d_Z <= d_ZERO) THEN
   ! 'Error: Zero or negative argument for lngamma'
   RETURN   ! ... error exit ...
END IF
d_Lanczos = d_ZERO
d_Tmp     = d_Z + d_SEVEN
DO j=9,2,-1
   d_Lanczos = d_Lanczos + da_A(j)/d_Tmp
   d_Tmp     = d_Tmp - d_ONE
END DO
d_Lanczos = d_Lanczos + da_A(1)
d_Lanczos = LOG(d_Lanczos) + d_LNSQRT2PI - (d_Z + d_SIXPT5) +        &
      (d_Z-d_HALF)*LOG(d_Z+d_SIXPT5)
RETURN   ! ... normal exit ...
END FUNCTION lngamma

!=============================================================================!

FUNCTION alnorm(d_X, l_Upper) RESULT(d_Fn_Val)
IMPLICIT NONE
!___Dummy Arguments:
REAL (KIND(0D0)), INTENT(IN) :: d_X
LOGICAL,          INTENT(IN) :: l_Upper
REAL (KIND(0D0)) :: d_Fn_Val
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     FUNCTION alnorm
! DESCRIPTION:
!     Algorithm AS66 Applied Statistics (1973) vol.22, no.3
!
!     Evaluates the tail area of the standardised normal curve
!     from d_X to infinity if l_Upper is .true. or
!     from minus infinity to d_X if l_Upper is .false.

! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!___Procedure Parameters:
REAL (KIND(0D0)), PARAMETER :: d_ZERO   =   0.0D0
REAL (KIND(0D0)), PARAMETER :: d_ONE    =   1.0D0
REAL (KIND(0D0)), PARAMETER :: d_HALF   =   0.5D0
REAL (KIND(0D0)), PARAMETER :: d_CON    =   1.28D0
! *** machine dependent constants (Alan Miller)
REAL (KIND(0D0)), PARAMETER :: d_LTONE  =   7.0D0
REAL (KIND(0D0)), PARAMETER :: d_UTZERO =  18.66D0
REAL (KIND(0D0)), PARAMETER :: d_P      =   0.398942280444D0
REAL (KIND(0D0)), PARAMETER :: d_Q      =   0.39990348504D0
REAL (KIND(0D0)), PARAMETER :: d_R      =   0.398942280385D0
REAL (KIND(0D0)), PARAMETER :: d_A1     =   5.75885480458D0
REAL (KIND(0D0)), PARAMETER :: d_A2     =   2.62433121679D0
REAL (KIND(0D0)), PARAMETER :: d_A3     =   5.92885724438D0
REAL (KIND(0D0)), PARAMETER :: d_B1     = -29.8213557807D0
REAL (KIND(0D0)), PARAMETER :: d_B2     =  48.6959930692D0
REAL (KIND(0D0)), PARAMETER :: d_C1     =  -3.8052D-8
REAL (KIND(0D0)), PARAMETER :: d_C2     =   3.98064794D-4
REAL (KIND(0D0)), PARAMETER :: d_C3     =  -0.151679116635D0
REAL (KIND(0D0)), PARAMETER :: d_C4     =   4.8385912808D0
REAL (KIND(0D0)), PARAMETER :: d_C5     =   0.742380924027D0
REAL (KIND(0D0)), PARAMETER :: d_C6     =   3.99019417011D0
REAL (KIND(0D0)), PARAMETER :: d_D1     =   1.00000615302D0
REAL (KIND(0D0)), PARAMETER :: d_D2     =   1.98615381364D0
REAL (KIND(0D0)), PARAMETER :: d_D3     =   5.29330324926D0
REAL (KIND(0D0)), PARAMETER :: d_D4     = -15.1508972451D0
REAL (KIND(0D0)), PARAMETER :: d_D5     =  30.789933034D0
!___Local Variables:
REAL (KIND(0D0)) :: d_Z, d_Y
LOGICAL :: l_Up
!___Intrinsic Procedures:
INTRINSIC :: EXP
!___Executable Statements:
l_Up = l_Upper
d_Z  = d_X
IF (d_Z <  d_ZERO) THEN
   l_Up = .NOT.l_Up
   d_Z  = -d_Z
END IF
IF (d_Z <= d_LTONE  .OR.  l_Up  .AND.  d_Z <= d_UTZERO) THEN
   d_Y = d_HALF*d_Z*d_Z
   IF (d_Z > d_CON) THEN
      d_Fn_Val = d_R*EXP(-d_Y)/(d_Z+d_C1+d_D1/(d_Z+d_C2+d_D2/(d_Z+d_C3+d_D3/  &
            (d_Z+d_C4+d_D4/(d_Z+d_C5+d_D5/(d_Z+d_C6))))))
    ELSE
      d_Fn_Val = d_HALF - d_Z*(d_P-d_Q*d_Y/(d_Y+d_A1+d_B1/(d_Y+d_A2+d_B2/     &
            (d_Y+d_A3))))
    END IF
ELSE
   d_Fn_Val = d_ZERO
END IF
IF (.NOT.l_Up) d_Fn_Val = d_ONE - d_Fn_Val
RETURN   ! ... normal exit ...
END FUNCTION alnorm

!=============================================================================!

FUNCTION gammad(d_X, d_P) RESULT(d_Fn_Val)
IMPLICIT NONE
!___Dummy Arguments:
REAL (KIND(0D0)), INTENT(IN) :: d_X
REAL (KIND(0D0)), INTENT(IN) :: d_P
REAL (KIND(0D0)) :: d_Fn_Val
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     FUNCTION gammad
! DESCRIPTION:
!     ALGORITHM AS239  APPL. STATIST. (1988) VOL. 37, NO. 3
!
!     Computation of the Incomplete Gamma Integral
!
!     Auxiliary functions required: ALOGAM = logarithm of the gamma
!     function, and ALNORM = algorithm AS66
!     N.B. Argument IFAULT has been removed (Alan Miller)

! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!___Procedure Parameters:
REAL (KIND(0D0)), PARAMETER  :: d_ZERO   =    0.0D0
REAL (KIND(0D0)), PARAMETER  :: d_ONE    =    1.0D0
REAL (KIND(0D0)), PARAMETER  :: d_TWO    =    2.0D0
REAL (KIND(0D0)), PARAMETER  :: d_OFLO   =    1.0D37
REAL (KIND(0D0)), PARAMETER  :: d_THREE  =    3.0D0
REAL (KIND(0D0)), PARAMETER  :: d_NINE   =    9.0D0
REAL (KIND(0D0)), PARAMETER  :: d_TOL    =    1.0D-14
REAL (KIND(0D0)), PARAMETER  :: d_XBIG   =    1.0D8
REAL (KIND(0D0)), PARAMETER  :: d_PLIMIT = 1000.0D0
REAL (KIND(0D0)), PARAMETER  :: d_ELIMIT =  -88.0D0
!___Local Variables:
REAL (KIND(0D0)) :: d_Arg, d_C, d_Rn, d_A, d_B, d_An
REAL (KIND(0D0)) :: d_Pn1, d_Pn2, d_Pn3, d_Pn4, d_Pn5, d_Pn6
!___Intrinsic Procedures:
INTRINSIC :: ABS, EXP, LOG, MIN, SQRT
!___Executable Statements:
d_Fn_Val = d_ZERO
! Check that we have valid values for d_X and d_P
IF (d_P <= d_ZERO  .OR.  d_X < d_ZERO) THEN
   ! 'AS239: Either P <= 0 or X < 0'
   RETURN   ! ... error exit ...
END IF
IF (d_X == d_ZERO) RETURN   ! ... error exit ...
! Use a normal approximation if d_P > dPLIMIT
IF (d_P > d_PLIMIT) THEN
   d_Pn1 = d_THREE*SQRT(d_P)*((d_X/d_P)**(d_ONE/d_THREE) + d_ONE /  &
         (d_NINE*d_P) - d_ONE)
   d_Fn_Val = alnorm(d_Pn1, .FALSE.) !* <File: "csgamma.f90: alnorm">
   RETURN   ! ... normal exit ...
END IF
! If d_X is extremely large compared to d_P then set d_Fn_Val = 1
IF (d_X > d_XBIG) THEN
   d_Fn_Val = d_ONE
   RETURN   ! ... normal exit ...
END IF
IF (d_X <= d_ONE  .OR.  d_X < d_P) THEN
   ! Use Pearson's series expansion.
   ! (Note that P is not large enough to force overflow in ALOGAM (or lngamma)).
   d_Arg = d_P*LOG(d_X) - d_X - lngamma(d_P+d_ONE) !* <File: "csgamma.f90: lngamma">
   d_A = d_P
   d_C = d_ONE
   d_Fn_Val = d_ONE
   DO
   call rchkusr()
      d_A = d_A + d_ONE
      d_C = d_C*d_X/d_A
      d_Fn_Val = d_Fn_Val + d_C
      IF (d_C <= d_TOL) EXIT
   END DO
   d_Arg    = d_Arg + LOG(d_Fn_Val)
   d_Fn_Val = d_ZERO
   IF (d_Arg >= d_ELIMIT) d_Fn_Val = EXP(d_Arg)
ELSE
   ! Use a continued fraction expansion
   d_Arg = d_P*LOG(d_X) - d_X - lngamma(d_P) !* <File: "csgamma.f90: lngamma">
   d_A   = d_ONE - d_P
   d_B   = d_A + d_X + d_ONE
   d_C   = d_ZERO
   d_Pn1 = d_ONE
   d_Pn2 = d_X
   d_Pn3 = d_X + d_ONE
   d_Pn4 = d_X*d_B
   d_Fn_Val = d_Pn3/d_Pn4
   DO
      d_A   = d_A + d_ONE
      d_B   = d_B + d_TWO
      d_C   = d_C + d_ONE
      d_An  = d_A*d_C
      d_Pn5 = d_B*d_Pn3 - d_An*d_Pn1
      d_Pn6 = d_B*d_Pn4 - d_An*d_Pn2
      IF (ABS(d_Pn6) > d_ZERO) THEN
         d_Rn = d_Pn5/d_Pn6
         IF (ABS(d_Fn_Val-d_Rn) <= MIN(d_TOL, d_TOL*d_Rn)) EXIT
         d_Fn_Val = d_Rn
      END IF
      d_Pn1 = d_Pn3
      d_Pn2 = d_Pn4
      d_Pn3 = d_Pn5
      d_Pn4 = d_Pn6
      IF (ABS(d_Pn5) >= d_OFLO) THEN ! If the terms are large,
         ! Then re-scale terms in continued fraction.
         d_Pn1 = d_Pn1/d_OFLO
         d_Pn2 = d_Pn2/d_OFLO
         d_Pn3 = d_Pn3/d_OFLO
         d_Pn4 = d_Pn4/d_OFLO
      END IF
      call rchkusr()
   END DO
   d_Arg    = d_Arg + LOG(d_Fn_Val)
   d_Fn_Val = d_ONE
   IF (d_Arg >= d_ELIMIT) d_Fn_Val = d_ONE - EXP(d_Arg)
END IF
RETURN   ! ... normal exit ...
END FUNCTION gammad

!=============================================================================!

FUNCTION CS_Prob(i_Ndf, d_Chi2) RESULT(d_Prob)
!___Imported Parameters and Variables:
        ! FOR SIMULATIONS ONLY:
USE simmod, ONLY: lo_IS_SIMUL    !SIM
        ! ^-- simulation info
USE ablparms, ONLY: i_SIM_UNIT   !SIM
        ! ^-- FOR SIMULATIONS ONLY
IMPLICIT NONE
!___Dummy Arguments:
INTEGER,          INTENT(IN) :: i_Ndf
REAL (KIND(0D0)), INTENT(IN) :: d_Chi2
REAL (KIND(0D0)) :: d_Prob
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     FUNCTION CS_Prob
! DESCRIPTION:
!     Calculate the chi-squared distribution function
!      i_Ndf  = number of degrees of freedom
!      d_Chi2 = chi-squared value
!      d_Prob = probability of chi-squared value > d_Chi2
!        Note: originally d_Prob was the probability of a chi-squared
!        value <= d_Chi2 (i.e. the left-hand tail area) - JDR  18 Apr 2000

! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!___Procedure Parameters:
REAL (KIND(0D0)), PARAMETER :: d_HALF = 0.5D0
REAL (KIND(0D0)), PARAMETER :: d_ONE  = 1.0D0
REAL (KIND(0D0)), PARAMETER :: d_ZERO = 0.0D0
REAL (KIND(0D0)), PARAMETER :: d_TOL  = 0.1D-15
!___Local Variables:
REAL (KIND(0D0)) :: d_X
REAL (KIND(0D0)) :: d_P
!___Intrinsic Procedures:
INTRINSIC :: HUGE, DBLE
!___Executable Statements:
IF (i_Ndf < 0  .OR.  d_Chi2 <= d_ZERO) THEN
   ! 'CS_Prob: Arguments must be greater than zero in value'
   d_Prob = -HUGE(d_ONE)
   RETURN   ! ... error exit ...
END IF
        ! FOR SIMULATIONS ONLY:
IF (lo_IS_SIMUL) THEN                !SIM
   CALL chisq(d_Chi2, i_Ndf, d_Prob) !SIM
   RETURN                            !SIM
END IF                               !SIM
        ! ^-- FOR SIMULATIONS ONLY
d_X    = d_HALF * d_Chi2
d_P    = d_HALF * DBLE(i_Ndf)
d_Prob = d_ONE - gammad(d_X, d_P) !* <File: "csgamma.f90: gammad">
! If d_Prob is nearly zero, use Mielke's CHISQ procedure to obtain
! a better(?) value.
IF (d_Prob < d_TOL) THEN
   CALL chisq(d_Chi2, i_Ndf, d_Prob)
END IF
RETURN   ! ... normal exit ...
END FUNCTION CS_Prob

!=============================================================================!

SUBROUTINE chisq(d_T, i_Ndf, d_PValue)
!___Imported Parameters and Variables:
USE blcmnmod, ONLY: d_PI
        ! ^-- Some global Blossom variables and parameters.
IMPLICIT NONE
!___Dummy Arguments:
REAL (KIND(0D0)), INTENT(IN)  :: d_T      ! Chi-square value
INTEGER,          INTENT(IN)  :: i_Ndf    ! Degrees of Freedom
REAL (KIND(0D0)), INTENT(OUT) :: d_PValue ! P-value returned
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     SUBROUTINE chisq
! DESCRIPTION:
!     This is Mielke's routine.  It was modified by JDR with explicit type
!     conversions and some Fortran 90 conventions for calling convenience,
!     as well as use of parameters to assure constant values don't change ;^).
!     The modified code (explicit type conversions) produces different
!     numbers after 6 to 10 significant digits. I don't know if this is
!     better, but it is closer to Blossom's original values.
!     Mielke's procedure works better(?) for extreme values of i_Ndf and d_T,
!     where the resultant d_PValue (P-value) is very small, and Blossom's
!     CS-Prob gives essentially 0.0D0 (gammad is 1.0D0).

! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!___Procedure Parameters:
REAL (KIND(0D0)), PARAMETER :: d_E1   =  0.31938153D0
REAL (KIND(0D0)), PARAMETER :: d_E2   = -0.356563782D0
REAL (KIND(0D0)), PARAMETER :: d_E3   =  1.781477937D0
REAL (KIND(0D0)), PARAMETER :: d_E4   = -1.821255978D0
REAL (KIND(0D0)), PARAMETER :: d_E5   =  1.330274429D0
REAL (KIND(0D0)), PARAMETER :: d_H    =  0.2316419D0
REAL (KIND(0D0)), PARAMETER :: d_ZERO =  0.0D0
REAL (KIND(0D0)), PARAMETER :: d_ONE  =  1.0D0
REAL (KIND(0D0)), PARAMETER :: d_TWO  =  2.0D0
!___Local Variables:
! REAL (KIND(0D0)) :: d_PI
REAL (KIND(0D0)) :: d_A, d_B, d_W
INTEGER :: i, j, k, l
!___Intrinsic Procedures:
! INTRINSIC ATAN
INTRINSIC :: DBLE, EXP, SQRT
!___Executable Statements:
IF (d_T < d_ZERO) THEN
   d_PValue = d_ONE
   GOTO 90000 ! (done).
END IF
IF((DBLE(i_Ndf)/d_TWO-DBLE(i_Ndf/2)) <= 0.1D0) THEN
   ! i_Ndf is EVEN:
   d_PValue = EXP(-d_T/d_TWO)
   IF (i_Ndf < 3) GOTO 90000  ! Small i_Ndf (done).
   k = (i_Ndf-2)/2
   d_B = d_ONE
   DO j=1,k
      l = j
      d_A = d_ONE
      DO i=1,l
         ! d_A = d_A * d_T/(d_TWO*DBLE(I))
         d_A = d_A*d_T/DBLE(2*i)
         call rchkusr()
      END DO
      d_B = d_B + d_A
   END DO
   d_PValue = d_PValue*d_B
ELSE
   ! i_Ndf is ODD:
   ! d_PI = ATAN(d_ONE)*4.0D0
   d_W  = d_ONE/(d_H*SQRT(d_T)+d_ONE)
   d_PValue = ((((d_E5*d_W+d_E4)*d_W+d_E3)*d_W+d_E2)*d_W+d_E1) *        &
         d_W*EXP(-d_T/d_TWO)/SQRT(d_PI/d_TWO)
   IF (i_Ndf < 2) GOTO 90000  ! Small i_Ndf (done).
   k = (i_Ndf-1)/2
   d_B = d_ZERO
   DO j=1,k
      l = j
      d_A = d_ONE
      DO i=1,l
         ! d_A = d_A * d_T/(d_TWO*DBLE(I)-d_ONE)
         d_A = d_A*d_T/DBLE(2*i-1)
         call rchkusr()
      END DO
      d_B = d_B + d_A
   END DO
   d_PValue = d_PValue + d_B*EXP(-d_T/d_TWO)/SQRT(d_PI*d_T/d_TWO)
END IF
90000 CONTINUE
RETURN
END SUBROUTINE chisq

!=============================================================================!

FUNCTION chk(s) RESULT(i)
INTEGER i
INTEGER l, m
CHARACTER (LEN=*), INTENT(IN) :: s
INTRINSIC :: IACHAR, LEN_TRIM
l=LEN_TRIM(s)
i=0
DO m=1,l
i=i+IACHAR(s(m:m))
END DO
RETURN
END FUNCTION chk

!=============================================================================!

END MODULE csgamma


!     ******************************************************************
!     mt19937ar.f90
!     United States Geological Survey
!
!     Created: 4/14/2005 5:09:40 PM
!     Author : Takuji Nishimura and Makoto Matsumoto;
!              Fortran: Josi Rui Faustino de Sousa
!     Last change: JDR 10/24/2006 1:25:52 PM
!     ******************************************************************
MODULE mt19937
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
! A C-program for MT19937, with initialization improved 2002/1/26.
! Coded by Takuji Nishimura and Makoto Matsumoto.
! Code converted to Fortran 95 by Josi Rui Faustino de Sousa
! Date: 2002-02-01
! Before using, initialize the state by using init_genrand(seed)
! or init_by_array(init_key, key_length).
! This library is free software.
! This library is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
! Copyright (C) 1997, 2002 Makoto Matsumoto and Takuji Nishimura.
! Any feedback is very welcome.
! http://www.math.keio.ac.jp/matumoto/emt.html
! email: matumoto@math.keio.ac.jp
!
! Commercial Use of Mersenne Twister 2001/4/6:
!     Until 2001/4/6, MT had been distributed under GNU Public License, but
!     after 2001/4/6, we decided to let MT be used for any purpose, including
!     commercial use.
! 2002-versions mt19937ar.c, mt19937ar-cok.c are considered to be usable freely.
!
! Matsumoto, M. and T. Nishimura. 1998. Mersenne Twister: A 623-Dimensionally
! Equidistributed Uniform Pseudo-Random Number Generator. ACM Transactions on
! Modeling and Computer Simulation 8:3-30.
!
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
IMPLICIT NONE
INTRINSIC :: BIT_SIZE
PRIVATE
PUBLIC :: init_genrand
PUBLIC :: genrand_int32
PUBLIC :: genrand_real1, genrand_real2, genrand_real3
INTEGER,  PARAMETER :: intg = SELECTED_INT_KIND(9)
INTEGER,  PARAMETER :: long = SELECTED_INT_KIND(18)
INTEGER,  PARAMETER :: flot = SELECTED_REAL_KIND(6, 37)
INTEGER,  PARAMETER :: dobl = SELECTED_REAL_KIND(15, 307)
INTEGER,  PUBLIC, PARAMETER :: wi = intg
INTEGER,  PUBLIC, PARAMETER :: wl = long
INTEGER,  PUBLIC, PARAMETER :: wr = dobl
! Period parameters
INTEGER (KIND=wi), PARAMETER :: n = 624_wi
INTEGER (KIND=wi), PARAMETER :: m = 397_wi
INTEGER (KIND=wi), PARAMETER :: hbs = BIT_SIZE(n)/2_wi
INTEGER (KIND=wi), PARAMETER :: qbs = hbs/2_wi
INTEGER (KIND=wi), PARAMETER :: tbs = 3_wi * qbs
INTEGER (KIND=wi) :: mt(n)          ! the array for the state vector
LOGICAL (KIND=wi) :: mtinit = .FALSE._wi ! means mt[N] is not initialized
INTEGER (KIND=wi) :: mti = n + 1_wi ! mti==N+1 means mt[N] is not initialized
CONTAINS

!==============================================================================

ELEMENTAL FUNCTION uiadd(a, b) RESULT(c)
IMPLICIT NONE
INTEGER (KIND=wi), INTENT(IN) :: a, b
INTEGER (KIND=wi) :: c
INTEGER (KIND=wi) :: a1, a2, b1, b2, s1, s2
INTRINSIC :: IBITS, IOR, ISHFT
a1 = IBITS(a, 0, hbs)
a2 = IBITS(a, hbs, hbs)
b1 = IBITS(b, 0, hbs)
b2 = IBITS(b, hbs, hbs)
s1 = a1 + b1
s2 = a2 + b2 + IBITS(s1, hbs, hbs)
c  = IOR(ISHFT(s2, hbs), IBITS(s1, 0, hbs))
END FUNCTION uiadd

!==============================================================================

ELEMENTAL FUNCTION uisub(a, b) RESULT(c)
IMPLICIT NONE
INTEGER (KIND=wi), INTENT(IN) :: a, b
INTEGER (KIND=wi) :: c
INTEGER (KIND=wi) :: a1, a2, b1, b2, s1, s2
INTRINSIC :: IBITS, IOR, ISHFT
a1 = IBITS(a, 0, hbs)
a2 = IBITS(a, hbs, hbs)
b1 = IBITS(b, 0, hbs)
b2 = IBITS(b, hbs, hbs)
s1 = a1 - b1
s2 = a2 - b2 + IBITS(s1, hbs, hbs)
c  = IOR(ISHFT(s2, hbs), IBITS(s1, 0, hbs))
END FUNCTION uisub

!==============================================================================

ELEMENTAL FUNCTION uimlt(a, b) RESULT(c)
IMPLICIT NONE
INTEGER (KIND=wi), INTENT(IN) :: a, b
INTEGER (KIND=wi) :: c
INTEGER (KIND=wi) :: a0, a1, a2, a3
INTEGER (KIND=wi) :: b0, b1, b2, b3
INTEGER (KIND=wi) :: p0, p1, p2, p3
INTRINSIC :: IBITS, IOR, ISHFT
a0 = IBITS(a, 0, qbs)
a1 = IBITS(a, qbs, qbs)
a2 = IBITS(a, hbs, qbs)
a3 = IBITS(a, tbs, qbs)
b0 = IBITS(b, 0, qbs)
b1 = IBITS(b, qbs, qbs)
b2 = IBITS(b, hbs, qbs)
b3 = IBITS(b, tbs, qbs)
p0 = a0*b0
p1 = a1*b0 + a0*b1 + IBITS(p0, qbs, tbs)
p2 = a2*b0 + a1*b1 + a0*b2 + IBITS(p1, qbs, tbs)
p3 = a3*b0 + a2*b1 + a1*b2 + a0*b3 + IBITS(p2, qbs, tbs)
c  = IOR(ISHFT(p1, qbs), IBITS(p0, 0, qbs))
c  = IOR(ISHFT(p2, hbs), IBITS(c, 0, hbs))
c  = IOR(ISHFT(p3, tbs), IBITS(c, 0, tbs))
END FUNCTION uimlt

!==============================================================================

SUBROUTINE init_genrand(s)
IMPLICIT NONE
INTEGER (KIND=wi), INTENT(IN) :: s
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
! initializes mt[N] with a seed
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
INTEGER (KIND=wi) :: i, mult_a
DATA mult_a /z'6C078965'/
INTRINSIC :: IAND, ISHFT, IEOR, IBITS
mtinit = .TRUE._wi
mt(1) = IBITS(s, 0, 32)
DO i=2,n,1
   mt(i) = IEOR(mt(i-1), ISHFT(mt(i-1), -30))
   mt(i) = uimlt(mt(i), mult_a)
   mt(i) = uiadd(mt(i), uisub(i, 1_wi))
   ! See Knuth TAOCP Vol2. 3rd Ed. P.106 for multiplier.
   ! In the previous versions, MSBs of the seed affect
   ! only MSBs of the array mt[].
   ! 2002/01/09 modified by Makoto Matsumoto
   mt(i) = IBITS(mt(i), 0, 32)
   ! for >32 bit machines
END DO
END SUBROUTINE init_genrand

!==============================================================================

FUNCTION genrand_int32( ) RESULT(y)
IMPLICIT NONE
INTEGER (KIND=wi) :: y
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
! generates a random number on [0,0xffffffff]-interval
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
INTEGER (KIND=wi) :: kk
INTEGER (KIND=wi) :: seed_d, matrix_b
INTEGER (KIND=wl) :: matrix_a, temper_a, temper_b
DATA seed_d   /z'5489'/
DATA matrix_a /z'9908B0DF'/
DATA matrix_b /z'0'/
DATA temper_a /z'9D2C5680'/
DATA temper_b /z'EFC60000'/
INTRINSIC :: IAND, ISHFT, IOR, IEOR, BTEST, IBSET, MVBITS
IF (mti > n) THEN ! generate N words at one time
   IF (.NOT. mtinit) CALL init_genrand(seed_d) ! if init_genrand( ) has not been called, a default initial seed is used
   DO kk=1,n-m,1
      y = IBITS(mt(kk+1), 0, 31)
      CALL MVBITS(mt(kk), 31, 1, y, 31)
      IF (BTEST(y, 0)) THEN
         mt(kk) = IEOR(IEOR(mt(kk+m), ISHFT(y, -1)), matrix_a)
      ELSE
         mt(kk) = IEOR(IEOR(mt(kk+m), ISHFT(y, -1)), matrix_b)
      END IF
      call rchkusr()
   END DO
   DO kk=n-m+1,n-1,1
      y = IBITS(mt(kk+1), 0, 31)
      CALL MVBITS(mt(kk), 31, 1, y, 31)
      IF (BTEST(y, 0)) THEN
         mt(kk) = IEOR(IEOR(mt(kk+m-n), ISHFT(y, -1)), matrix_a)
      ELSE
         mt(kk) = IEOR(IEOR(mt(kk+m-n), ISHFT(y, -1)), matrix_b)
      END IF
      call rchkusr()
   END DO
   y = IBITS(mt(1), 0, 31)
   CALL MVBITS(mt(n), 31, 1, y, 31)
   IF (BTEST(y, 0)) THEN
      mt(kk) = IEOR(IEOR(mt(m), ISHFT(y, -1)), matrix_a)
   ELSE
      mt(kk) = IEOR(IEOR(mt(m), ISHFT(y, -1)), matrix_b)
   END IF
   mti = 1_wi
END IF
y = mt(mti)
mti = mti + 1_wi
! Tempering
y = IEOR(y, ISHFT(y, -11))
y = IEOR(y, IAND(ISHFT(y, 7), temper_a))
y = IEOR(y, IAND(ISHFT(y, 15), temper_b))
y = IEOR(y, ISHFT(y, -18))
if(abs(y)>2137483647) then
	
end if
END FUNCTION genrand_int32

!==============================================================================


FUNCTION genrand_real1( ) RESULT(r)
IMPLICIT NONE
REAL (KIND=wr) :: r
double precision unifrnd
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
! generates a random number on [0,1]-real-interval
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

call rndstart()
r=unifrnd()
call rndend()

END FUNCTION genrand_real1

!==============================================================================

FUNCTION genrand_real2( ) RESULT(r)
IMPLICIT NONE
REAL (KIND=wr) :: r
double precision unifrnd
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
! generates a random number on [0,1)-real-interval
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
call rndstart()
r=unifrnd()
call rndend()

END FUNCTION genrand_real2

!==============================================================================

! These real versions are due to Isaku Wada, 2002/01/09 added
FUNCTION genrand_real3( ) RESULT(r)
IMPLICIT NONE
REAL (KIND=wr) :: r
double precision unifrnd
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
! generates a random number on (0,1)-real-interval  (inclusive of limits)
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
call rndstart()
r=unifrnd()
call rndend()

END FUNCTION genrand_real3

!==============================================================================



!==============================================================================

END MODULE mt19937


!==============================================================================
MODULE binomod
!dd USE debugmod                                                       ! debug!
IMPLICIT NONE
REAL, PRIVATE, PARAMETER :: ZERO = 0.0
REAL, PRIVATE, PARAMETER :: HALF = 0.5
REAL, PRIVATE, PARAMETER :: ONE = 1.0
CONTAINS
!==============================================================================

FUNCTION random_binomial(n, pp, l_First) RESULT(i_FnResult)
!___Imported Variables and Parameters:
!___Imported procedures:
USE mt19937, ONLY: genrand_real3, genrand_real1
        ! ^-- Mersenne Twister Pseudo Random Number Generator
! FUNCTION btpec90(n, pp, l_First) RESULT(i_FnResult)
IMPLICIT NONE
!___Dummy Arguments:
INTEGER, INTENT(IN) :: n
REAL,    INTENT(IN) :: pp
LOGICAL, INTENT(IN) :: l_First
!___Function Result:
INTEGER :: i_FnResult

! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
REAL, PARAMETER :: r_ZERO = ZERO
REAL, PARAMETER :: r_HALF = HALF
REAL, PARAMETER ::  r_ONE = ONE
REAL, PARAMETER :: r_ONE_SIXTH = 1.0/6.0
!___Local Variables:
REAL :: alv, amaxp, f, f1, f2, u, v, w, w2, x, x1, x2, ynorm, z, z2
REAL, SAVE :: al, c, ffm, fm, g, p, p1, p2, p3, p4, q, qn, r, xl, xll, &
                 xlr, xm,  xnp, xnpq, xr
INTEGER :: i, ix, ix1, k, mp
INTEGER :: icnt, ic, icr, icm, iseedsize
INTEGER, ALLOCATABLE :: new_seed(:) ! (2)
INTEGER, SAVE :: m
!___Intrinsic Procedures:
INTRINSIC :: ABS, INT, LOG, MIN, RANDOM_NUMBER, RANDOM_SEED, SQRT
!___Executable Statements:
        ! *****SETUP, PERFORM ONLY WHEN PARAMETERS CHANGE
IF (l_First) THEN
   p = MIN(pp, r_ONE-pp)
   q = r_ONE - p
   xnp = n*p
END IF
        ! *****DETERMINE APPROPRIATE ALGORITHM AND WHETHER SETUP IS NECESSARY
IF (xnp > 30.0) THEN
   IF (l_First) THEN
      ffm = xnp + p
      m = ffm
      fm = m
      xnpq = xnp*q
      p1 = INT(2.195*SQRT(xnpq)-4.6*q) + r_HALF
      xm = fm + r_HALF
      xl = xm - p1
      xr = xm + p1
      c = 0.134 + 20.5/(15.3+fm)
      al = (ffm-xl)/(ffm-xl*p)
      xll = al*(r_ONE+r_HALF*al)
      al = (xr-ffm)/(xr*q)
      xlr = al*(r_ONE+r_HALF*al)
      p2 = p1*(r_ONE+c+c)
      p3 = p2 + c/xll
      p4 = p3 + c/xlr
   END IF
        ! *****GENERATE VARIATE, Binomial mean at least 30.
      loop20: &
   DO
!     Mod - Add option to use Mersenne Twister Pseudo Random Number Generator
!            - JDR Apr 2005

         u = genrand_real1( ) ! [0,1]
         v = genrand_real1( ) ! [0,1]

      u = u*p4
      IF (u <= p1) THEN    !     TRIANGULAR REGION
         ix = xm - p1*v + u
         EXIT loop20
      END IF
      IF (u <= p2) THEN    !     PARALLELOGRAM REGION
         x = xl + (u-p1)/c
         v = v*c + r_ONE - ABS(xm-x)/p1
         IF (v > r_ONE  .OR.  v <= r_ZERO) CYCLE loop20
         ix = x
      ELSE
         IF (u <= p3) THEN !     LEFT (exponential) TAIL
            ix = xl + LOG(v)/xll
            IF (ix < 0) CYCLE loop20
            v = v*(u-p2)*xll
         ELSE              !     RIGHT (exponential) TAIL
            ix = xr - LOG(v)/xlr
            IF (ix > n) CYCLE loop20
            v = v*(u-p3)*xlr
         END IF
      END IF
        ! *****DETERMINE APPROPRIATE WAY TO PERFORM ACCEPT/REJECT TEST
      k = ABS(ix-m)
      IF (k <= 20  .OR.  k >= xnpq/2-1) THEN
        !     EXPLICIT EVALUATION    (start the search from the mode)
         f = r_ONE
         r = p/q
         g = (n+1)*r
         IF (m < ix) THEN
            mp = m + 1
            DO i=mp,ix
               f = f*(g/i-r)
               call rchkusr()
            END DO
         ELSE IF (m > ix) THEN
            ix1 = ix + 1
            DO i=ix1,m
               f = f/(g/i-r)
               call rchkusr()
            END DO
         END IF
         IF (v > f) THEN
            CYCLE loop20
         ELSE
            EXIT loop20
         END IF
      END IF
        !     SQUEEZING USING UPPER AND LOWER BOUNDS ON LOG(F(X))
        ! amaxp=(k/xnpq)*((k*(k/3.0+0.625)+0.1666666666666)/xnpq+r_HALF)
      amaxp = (k/xnpq)*((k*(k/3.0 + 0.625) + r_ONE_SIXTH)/xnpq + r_HALF)
      ynorm = -k*k/(2.0*xnpq)
      alv = LOG(v)
      IF (alv < ynorm-amaxp) EXIT loop20
      IF (alv > ynorm+amaxp) CYCLE loop20
        !     STIRLING'S (actually de Moivre's) FORMULA TO MACHINE ACCURACY FOR
        !     THE FINAL ACCEPTANCE/REJECTION TEST
      x1 = ix + 1
      f1 = fm + r_ONE
      z = n + 1 - fm
      w = n - ix + r_ONE
      z2 = z*z
      x2 = x1*x1
      f2 = f1*f1
      w2 = w*w
      IF (alv-(xm*LOG(f1/x1)+(n-m+r_HALF)*LOG(z/w)+(ix-m)*LOG(w*p/(x1*q)) + &
         (13860.0-(462.0-(132.0-(99.0 - 140.0/f2)/f2)/f2)/f2)/f1/166320.0 + &
         (13860.0-(462.0-(132.0-(99.0 - 140.0/z2)/z2)/z2)/z2)/z/166320.0  + &
         (13860.0-(462.0-(132.0-(99.0 - 140.0/x2)/x2)/x2)/x2)/x1/166320.0 + &
         (13860.0-(462.0-(132.0-(99.0 - 140.0/w2)/w2)/w2)/w2)/w/166320.0) > &
            r_ZERO) THEN
         CYCLE loop20
      ELSE
         EXIT loop20
      END IF
      call rchkusr()
   END DO &
      loop20
ELSE    !     INVERSE CDF LOGIC FOR MEAN LESS THAN 30
   IF (l_First) THEN
      qn = q**n
      r = p/q
      g = r*(n+1)
   END IF
      loop90: &
   DO
      ix = 0
      f = qn
!     Mod - Add option to use Mersenne Twister Pseudo Random Number Generator
!            - JDR Apr 2005

         u = genrand_real3( )

      DO
         IF (u < f) EXIT
         IF (ix > 110) CYCLE loop90
         u = u - f
         ix = ix + 1
         f = f*(g/ix-r)
         call rchkusr()
      END DO
      EXIT loop90
   END DO &
      loop90
END IF
IF (pp > r_HALF) ix = n - ix
i_FnResult = ix
RETURN
END FUNCTION random_binomial

!==============================================================================
FUNCTION random_binomialx(n, p, first) RESULT(ival)
!___Imported Variables and Parameters:
!___Imported Procedures:
USE mt19937, ONLY: genrand_real3
        ! ^-- Mersenne Twister Pseudo Random Number Generator
IMPLICIT NONE
!___Dummy Arguments:
INTEGER, INTENT(IN) :: n
REAL,    INTENT(IN) :: p
LOGICAL, INTENT(IN) :: first
!___Function Result:
INTEGER :: ival

!___Local Variables:
REAL :: u, pd, pu
REAL :: bin_prob
REAL, SAVE :: odds_ratio, p_r
INTEGER :: r
INTEGER :: ru, rd
INTEGER, SAVE :: r0
!___Statement Function:
bin_prob(n,p,r)=EXP(lngamma(DBLE(n+1))-lngamma(DBLE(r+1))-lngamma(DBLE(n-r+1))+r*LOG(p)+(n-r)*LOG(ONE-p))
IF (first) THEN
   r0 = (n+1)*p
   p_r = bin_prob(n, p, r0)
   odds_ratio = p/(ONE-p)
END IF

   u = genrand_real3( )

u = u - p_r
IF (u < ZERO) THEN
   ival = r0
   RETURN
END IF
pu = p_r
ru = r0
pd = p_r
rd = r0
DO
   rd = rd - 1
   IF (rd >= 0) THEN
      pd = pd*(rd+1)/(odds_ratio*(n-rd))
      u = u - pd
      IF (u < ZERO) THEN
         ival = rd
       RETURN
      END IF
   END IF
   ru = ru + 1
   IF (ru <= n) THEN
      pu = pu*(n-ru+1)*odds_ratio/ru
      u = u - pu
      IF (u < ZERO) THEN
         ival = ru
         RETURN
      END IF
   END IF
   call rchkusr()
END DO
        !     This point should not be reached, but just in case:
ival = r0
RETURN
CONTAINS
!======================================
   FUNCTION lngamma(x) RESULT(fn_val)
   IMPLICIT NONE
!___Dummy Arguments:
   REAL (KIND(0D0)), INTENT(IN) :: x
!___Function Result:
   REAL (KIND(0D0))             :: fn_val
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
! Logarithm to base e of the gamma function.

!___Local Parameters:
   REAL (KIND(0D0)), PARAMETER :: ZERO = 0.0D0
   REAL (KIND(0D0)), PARAMETER :: HALF = 0.5D0
   REAL (KIND(0D0)), PARAMETER :: ONE = 1.0D0
   REAL (KIND(0D0)), PARAMETER :: a1 = -4.166666666554424D-02
   REAL (KIND(0D0)), PARAMETER :: a2 =  2.430554511376954D-03
   REAL (KIND(0D0)), PARAMETER :: a3 = -7.685928044064347D-04
   REAL (KIND(0D0)), PARAMETER :: a4 =  5.660478426014386D-04
   REAL (KIND(0D0)), PARAMETER :: lnrt2pi = 9.189385332046727D-1
   REAL (KIND(0D0)), PARAMETER :: pi = 3.141592653589793D0
!___Local variables:
   REAL (KIND(0D0)) :: temp, arg, d_product
   LOGICAL:: reflect
!___Executable Statements:
        !       lngamma is not defined if x = 0 or a negative integer.
   IF (x > ZERO) GO TO 10
   IF (x /= INT(x)) GO TO 10
   fn_val = ZERO
   RETURN
        !       If x < 0, use the reflection formula:
        !               gamma(x) * gamma(1-x) = pi * cosec(pi.x)
   10 CONTINUE
   reflect = (x < ZERO)
   IF (reflect) THEN
      arg = ONE - x
   ELSE
      arg = x
   END IF
        !       Increase the argument, if necessary, to make it > 10.
   d_product = ONE
   DO
      IF (arg > 10.0D0) EXIT
      d_product = d_product * arg
      arg = arg + ONE
      call rchkusr()
   END DO
        !  Use a polynomial approximation to Stirling's formula.
        !  N.B. The real Stirling's formula is used here, not the simpler, but less
        !       accurate formula given by De Moivre in a letter to Stirling, which
        !       is the one usually quoted.
   arg = arg - HALF
   temp = ONE/arg**2
   fn_val = lnrt2pi + arg*(LOG(arg) - ONE + &
         (((a4*temp+a3)*temp+a2)*temp+a1)*temp) - LOG(d_product)
   IF (reflect) THEN
      temp = SIN(pi*x)
      fn_val = LOG(pi/temp) - fn_val
   END IF
   RETURN
   END FUNCTION lngamma

END FUNCTION random_binomialx
END MODULE binomod

!     ******************************************************************
!     pv_modul.f90
!     Author: Jon D. Richards    U.S. Geological Survey
!     Last change: JDR 4/21/2006 1:49:17 PM
!     ******************************************************************
!#/WINTERACTER/ ! This Comment means it is for Windows (WINTERACTER) version only.             !#/WINTERACTER/
! This Comment means it is for Console (CON) version only.                     !#/CON/
!  PVALUE    <File: "pv_modul.f90: pvalue">
!  E         <File: "pv_modul.f90: e">
!  COVPVALUE <File: "pv_modul.f90: covpvalue">

!=============================================================================!

MODULE pv_modul
IMPLICIT NONE
SAVE
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
CONTAINS
!     PVALUE    <File: "pv_modul.f90: pvalue">
!     E         <File: "pv_modul.f90: e">
!     COVPVALUE <File: "pv_modul.f90: covpvalue">

FUNCTION pvalue(d_T, d_Gam) RESULT(d_PValue)
!___Imported Parameters and Variables:
USE blcmnmod, ONLY: d_PI
        ! ^-- Some global Blossom variables and parameters.
!___Imported Procedures:
!__(none)__
IMPLICIT NONE
!___Dummy Arguments:
REAL (KIND(0D0)), INTENT(IN) :: d_T
REAL (KIND(0D0)), INTENT(IN) :: d_Gam
!___Returned Result:
REAL (KIND(0D0)) :: d_PValue
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!      FUNCTION pvalue
! DESCRIPTION:
!      calculates the probability that a value
!      of d_T is less than or equal to the observed value of d_T.
! LANGUAGE:
!      Fortran 90/95
! AUTHOR:
!      Dr. Paul Mielke, Colorado State University
!        http://www.stat.colostate.edu/~mielke/mielkesoftware.html
!   Modified to Fortran 90 by
!     Jon D. Richards, Operations Research Analyst    jon_richards@usgs.gov
!     Fort Collins Science Center                  http://www.fort.usgs.gov
!     U.S. Geological Survey          -          Biological Resources Division
! INVOKED BY:
!     mrpp     <File: "mrppmod.f90: mrpp">     module
!     mrspdist <File: "mrspmod.f90: mrspdist"> module
!     ptmp     <File: "mrppmod.f90: ptmp">     module
!     sptmp    <File: "mrppmod.f90: sptmp">    module
! INVOKES:
!     e        <File: "pv_modul.f90: e">       modules
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!___Procudure Parameters:
REAL (KIND(0D0)), PARAMETER :: d_ZERO =  0.0D0
REAL (KIND(0D0)), PARAMETER ::  d_ONE =  1.0D0
REAL (KIND(0D0)), PARAMETER ::   d_G1 =  0.0375D0  ! WAS d_G1 = 0.045D0
REAL (KIND(0D0)), PARAMETER ::   d_G2 =  0.075D0   ! WAS d_G2 = 0.09D0
REAL (KIND(0D0)), PARAMETER ::   d_G3 =  0.0125D0  ! WAS d_G3 = 0.015D0
REAL (KIND(0D0)), PARAMETER ::   d_E1 =  0.31938153D0
REAL (KIND(0D0)), PARAMETER ::   d_E2 = -0.356563782D0
REAL (KIND(0D0)), PARAMETER ::   d_E3 =  1.781477937D0
REAL (KIND(0D0)), PARAMETER ::   d_E4 = -1.821255978D0
REAL (KIND(0D0)), PARAMETER ::   d_E5 =  1.330274429D0
REAL (KIND(0D0)), PARAMETER ::    d_H =  0.2316419D0
INTEGER,          PARAMETER :: i_UBND =  199       ! WAS something else
!___Local Variables:
! REAL (KIND(0D0)) :: d_PI
REAL (KIND(0D0)) :: d_R, d_D, dF, dU, d_A, d_B
REAL (KIND(0D0)) :: d_G, d_H0, d_H1, d_H1n, d_H2, d_H3, d_W, d_X, d_Y
INTEGER :: i
!___Intrinsic Procedures:
! INTRINSIC ATAN
INTRINSIC :: ABS, DBLE, LOG, SQRT
!___Executable Statements:
! d_PI = ATAN(d_ONE)*4.0D0
IF (ABS(d_Gam) >= 0.001D0) THEN
   d_R = 2.0D0/ABS(d_Gam)
   d_D = d_R*d_R
   dF = d_R*d_R + 10.0D0
   DO i=1,9
       d_D = d_D*(d_R*d_R+DBLE(i))
   END DO
   dU  = (dF*2.0D0 - 1.0D0)*LOG(dF)/2.0D0 - dF +                              &
         LOG(d_PI*2.0D0)/2.0D0 - LOG(d_D)        +                            &
         d_ONE/(dF*12.0D0) - d_ONE/(dF*dF*dF*360.0D0)
   d_A  = d_R*d_R - d_ONE
   d_B  = d_R*d_R*(LOG(d_R)-d_ONE) - dU
   d_H1 = d_ZERO
   d_H2 = d_ZERO
   d_W  = -1.99D0/d_Gam
   IF (d_Gam >= d_ZERO) THEN
      IF (d_T < d_W) THEN
         d_PValue = d_ZERO
         RETURN   ! ... normal exit ...
      END IF
      d_X = d_T
      d_Y = d_T + 15.0D0
      DO i=1,i_UBND
         d_G  = d_A*LOG(d_R+d_X+d_G1*DBLE(2*i-1)) - &
               d_R*(d_X+d_G1*DBLE(2*i-1)) + d_B
         d_H1 = d_H1 + e(d_G)
         d_G  = d_A*LOG(d_R+d_X+d_G2*DBLE(i))     - &
               d_R*(d_X+d_G2*DBLE(i))     + d_B
         d_H2 = d_H2 + e(d_G)
         call rchkusr()
      END DO
      d_G      = d_A*LOG(d_R+d_X+d_G1*399.0D0) - d_R*(d_X+d_G1*399.0D0) + d_B
      d_H1n    = e(d_G)
      d_G      = d_A*LOG(d_R+d_X)              - d_R*d_X                + d_B
      d_H0     = e(d_G)
      d_G      = d_A*LOG(d_R+d_Y)              - d_R*d_Y                + d_B
      d_H3     = e(d_G)
      d_PValue = d_ONE - d_G3*(d_H0+(d_H1+d_H1n)*4.0D0+d_H2*2.0D0+d_H3)
      RETURN   ! ... normal exit ...
   END IF
   IF (d_T > d_W) THEN
      d_PValue = d_ONE
      RETURN   ! ... normal exit ...
   END IF
   d_X = d_T - 15.0D0
   d_Y = d_T
   DO i=1,i_UBND
      d_G  = d_A*LOG(d_R-d_X-d_G1*DBLE(2*i-1)) + d_R*(d_X+d_G1*DBLE(2*i-1)) + &
            d_B
      d_H1 = d_H1 + e(d_G)
      d_G  = d_A*LOG(d_R-d_X-d_G2*DBLE(i))     + d_R*(d_X+d_G2*DBLE(i))     + &
            d_B
      d_H2 = d_H2 + e(d_G)
      call rchkusr()
   END DO
   d_G      = d_A*LOG(d_R-d_X-d_G1*399.0D0) + d_R*(d_X+d_G1*399.0D0) + d_B
   d_H1n    = e(d_G)
   d_G      = d_A*LOG(d_R-d_X)              + d_R*d_X                + d_B
   d_H0     = e(d_G)
   d_G      = d_A*LOG(d_R-d_Y)              + d_R*d_Y                + d_B
   d_H3     = e(d_G)
   d_PValue = d_G3*(d_H0+(d_H1+d_H1n)*4.0D0+d_H2*2.0D0+d_H3)
   RETURN   ! ... normal exit ...
END IF
d_W = d_ONE/(d_H*ABS(d_T)+d_ONE)
d_G = -d_T*d_T/2.0D0
d_PValue = ((((d_E5*d_W+d_E4)*d_W+d_E3)*d_W+d_E2)*d_W+d_E1)*d_W*e(d_G) /      &
      SQRT(d_PI*2.0D0)
IF (d_T > d_ZERO) THEN
   d_PValue = d_ONE - d_PValue
END IF
RETURN   ! ... normal exit ...
END FUNCTION pvalue

!=============================================================================!

FUNCTION e(d_G)  RESULT(d_E)
!___Imported Parameters and Variables:
!__(none)__
!___Imported Procedures:
!__(none)__
! Modified - Limit for EXP here is -708. JDR  22 Feb 1999
IMPLICIT NONE
!___Dummy Arguments:
REAL (KIND(0D0)), INTENT(IN) :: d_G
!___Returned Result:
REAL (KIND(0D0)) :: d_E
!___Intrinsic Procedures:
INTRINSIC :: EXP
!___Executable Statements:
d_E = 0.0D0
IF (d_G >= -708.0D0) then
   d_E = EXP(d_G)
END IF
RETURN   ! ... normal exit ...
END FUNCTION e

!=============================================================================!

FUNCTION covpvalue(d_Z, d_Gam) RESULT(d_PValue)
!___Imported Parameters and Variables:
USE blcmnmod, ONLY: d_PI
        ! ^-- Some global Blossom variables and parameters.
!___Imported Procedures:
!__(none)__
IMPLICIT NONE
!___Dummy Arguments:
REAL (KIND(0D0)), INTENT (IN)  :: d_Z
REAL (KIND(0D0)), INTENT (IN)  :: d_Gam
!___Returned Result:
REAL (KIND(0D0)) :: d_PValue
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     SUBROUTINE covpvalue
! DESCRIPTION:
!     Calculates the probability of a value of d_Z being
!     GREATER THAN or EQUAL TO the observed value of d_Z.
!         ________________________________________
!        (                                        )
!        (   NOTE: This is DIFFERENT from the     )
!        (   pvalue function in module pv_modul   )
!        (   in that the latter calculates the    )
!        (   probability that a value of d_T is    )
!        (   LESS THAN or EQUAL TO observed d_T.   )
!        (   DO NOT replace this with a call      )
!        (   to the FUNCTION pvalue in pv_modul.  )
!        (________________________________________)
!
! LANGUAGE:
!     Fortran 90/95
! AUTHOR:
!     Dr. Paul W. Mielke, Jr., Dept of Statistics, Colorado State University
!        http://www.stat.colostate.edu/~mielke/mielkesoftware.html
!     Modifications:
!        Jon D. Richards, Operations Research Analyst    jon_richards@usgs.gov
!        Fort Collins Science Center                  http://www.fort.usgs.gov
!        U.S. Geological Survey  -  Biological Resources Division
! INVOKED BY:
!     gsect1   <File: "covermod.f90: gsect1"> module
!     ksgf     <File: "covermod.f90: ksgf">   module
! INVOKES:
!     e        <File: "pv_modul.f90: e">      module
! FILES:
!     -
! DEVICES:
!     -
! MODIFICATION HISTORY:
!   CODE HISTORY:
!     Mod 03/90 - Modified for Blossom
!     Modified 11/93 for Lahey F77L/EM32 Compiler (Extended and virtual
!              memory version for 386/486 and above. - JDR
!     Mod 09-94 for Lahey Fortran 90 LF90 - JDR
!     Mod - Renamed, moved to module. - JDR  20 Oct 1999
!   COMMENT HISTORY:
!     Updated 7 July 1999
!     Updated - JDR  20 Oct 1999
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!___Procedure Parameters:
REAL (KIND(0D0)), PARAMETER ::   d_ZERO =  0.0D0
REAL (KIND(0D0)), PARAMETER ::    d_ONE =  1.0D0
REAL (KIND(0D0)), PARAMETER ::    d_TWO =  2.0D0
REAL (KIND(0D0)), PARAMETER ::   d_FOUR =  4.0D0
REAL (KIND(0D0)), PARAMETER :: d_TWELVE = 12.0D0
REAL (KIND(0D0)), PARAMETER ::     d_E1 =  0.31938153D0
REAL (KIND(0D0)), PARAMETER ::     d_E2 = -0.356563782D0
REAL (KIND(0D0)), PARAMETER ::     d_E3 =  1.781477937D0
REAL (KIND(0D0)), PARAMETER ::     d_E4 = -1.821255978D0
REAL (KIND(0D0)), PARAMETER ::     d_E5 =  1.330274429D0
REAL (KIND(0D0)), PARAMETER ::     d_G1 =  0.0375D0
REAL (KIND(0D0)), PARAMETER ::     d_G2 =  0.075D0
REAL (KIND(0D0)), PARAMETER ::     d_G3 =  0.0125D0
REAL (KIND(0D0)), PARAMETER ::      d_H =  0.2316419D0
!___Local Variables:
REAL (KIND(0D0)) :: d_2Pi, A, B, C, D, F, G
REAL (KIND(0D0)) :: H0, H1, H2, H3, H4
REAL (KIND(0D0)) :: R, U, W, X, Y
INTEGER :: i
!___Intrinsic Procedures:
! INTRINSIC ATAN
INTRINSIC :: ABS, DBLE, LOG, SQRT

d_2Pi = 2.0D0*d_PI
IF (ABS(d_Gam) >= 0.01D0) THEN
   R = d_TWO / ABS(d_Gam)
   D = R*R
   DO i=1,9
      D = D*(R*R+DBLE(i))
   END DO
   F = R*R + 10.0D0
   U = (F*d_TWO-d_ONE)*LOG(F)/d_TWO - F + LOG(d_2Pi)/d_TWO -                  &
        LOG(D)+d_ONE/(F*d_TWELVE) - d_ONE/(F*F*F*360.0D0)
   H1 = d_ZERO
   H2 = d_ZERO
   A  = R*R - d_ONE
   B  = R*R*(LOG(R)-d_ONE) - U
   W  = -1.99D0/d_Gam
   IF (d_Gam >= d_ZERO) THEN
      IF (d_Z < W) THEN
         d_PValue = d_ONE
         RETURN   ! ... normal exit ...
      END IF
      X = d_Z
      Y = d_Z + 15.0D0
      DO i=1,199
         G = A*LOG(R+X+d_G1*DBLE(i*2-1))-R*(X+d_G1*DBLE(i*2-1)) + B
         H1 = H1 + e(G)  !* <File: "pv_modul.f90: e">
         G = A*LOG(R+X+d_G2*DBLE(i)) - R*(X+d_G2*DBLE(i)) + B
         H2 = H2 + e(G)
      END DO
      G = A*LOG(R+X) - R*X + B
      H0 = e(G)
      G = A*LOG(R+Y) - R*Y + B
      H3 = e(G)
      G = A*LOG(R+X+d_G1*399.0D0) - R*(X+d_G1*399.0D0) + B
      H4 = e(G)
      d_PValue = d_G3*(H0+(H1+H4)*d_FOUR+H2*d_TWO+H3)
      RETURN   ! ... normal exit ...
   END IF
   IF (d_Z > W) THEN
      d_PValue = d_ZERO
      RETURN   ! ... normal exit ...
   END IF
   X = d_Z - 15.0D0
   Y = d_Z
   DO i=1,199
      G = A*LOG(R-X-d_G1*DBLE(i*2-1)) + R*(X+d_G1*DBLE(i*2-1)) + B
      H1 = H1 + e(G)
      G = A*LOG(R-X-d_G2*DBLE(i)) + R*(X+d_G2*DBLE(i)) + B
      H2 = H2 + e(G)
   END DO
   G = A*LOG(R-X) + R*X + B
   H0 = e(G)
   G = A*LOG(R-Y) + R*Y + B
   H3 = e(G)
   G = A*LOG(R-X-d_G1*399.0D0) + R*(X+d_G1*399.0D0) + B
   H4 = e(G)
   d_PValue = d_ONE - d_G3*(H0+(H1+H4)*d_FOUR+H2*d_TWO+H3)
   RETURN   ! ... normal exit ...
END IF
d_PValue = d_ZERO
G = -d_Z*d_Z/d_TWO
IF (G < -708.0D0) THEN
   IF (d_Z < d_ZERO) d_PValue = d_ONE - d_PValue
   RETURN   ! ... normal exit ...
END IF
A = 4.2D0
U = ABS(d_Z)
IF (U <= A) THEN
   W  =  d_ONE / (d_H*ABS(d_Z)+d_ONE)
   d_PValue = ((((d_E5*W+d_E4)*W+d_E3)*W+d_E2)*W+d_E1)*W*e(G) / SQRT(d_2Pi)
ELSE
   B = 2.2D0
   C = 4.9D0
   d_PValue = e(G)*(d_ONE/U-d_ONE/U**3+B/U**C) / SQRT(d_2Pi)
END IF
IF (d_Z < d_ZERO) d_PValue = d_ONE - d_PValue
RETURN   ! ... normal exit ...
END FUNCTION covpvalue

!=============================================================================!

END MODULE pv_modul

!     ******************************************************************
!     missvmod.f90
!     Author: Jon D. Richards    U.S. Geological Survey
!     Last change: JDR 2004-04-20 23:03:13
!     ******************************************************************
!#/WINTERACTER/ ! This Comment means it is for Windows (WINTERACTER) version only.             !#/WINTERACTER/
! This Comment means it is for Console (CON) version only.                     !#/CON/
!=============================================================================!
MODULE missvmod
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     MODULE missvmod
! DESCRIPTION:
!     Missing Value values to use for and test missing values.
!     missvmod derived from block data common blocks /missval/ and /missing/.
!     Missing Value (SYSTAT convention, which is adequate.  Blossom sees

! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
IMPLICIT NONE
SAVE
REAL (KIND(0D0)),  PARAMETER :: d_MISSING_VALUE = -0.100000000000000D+037
REAL (KIND(0.0)),  PARAMETER :: r_MISSING_VALUE = -0.100000E+37
CHARACTER (LEN=*), PARAMETER :: ch_MISSING_VALUE   = "-0.10000E+37"
CHARACTER (LEN=*), PARAMETER :: ch_d_MISSING_VALUE = "-0.100000000000000D+037"
CHARACTER (LEN=*), PARAMETER :: ch_r_MISSING_VALUE = "-0.100000E+37"
END MODULE missvmod

!     ******************************************************************
!     mrolamod.f90
!     Author: Jon D. Richards    U.S. Geological Survey
!     Last change: JDR 8/16/2007 5:26:04 PM
!     ******************************************************************
!#/WINTERACTER/ ! This Comment means it is for Windows (WINTERACTER) version only.             !#/WINTERACTER/
! This Comment means it is for Console (CON) version only.                     !#/CON/
!=============================================================================!
MODULE mrolamod
!___Imported Parameters and Variables:
USE ablparms, ONLY: i_MAX_ELEM_LEN, i_MAX_N_VARS, i_MAX_MR_VARS,              &
      i_MAX_LAD_VARS, i_MAX_SP_VARS, i_MAX_CMD_LENGTH, i_MAX_LEN_VAR_NAME,    &
      i_NUM_LAD_CMDS, i_DP_OUT_WIDTH, i_NUM_LAD_CMDS
        ! ^-- Some high-level Blossom parameters.
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     MODULE mrolamod
! DESCRIPTION:
!     Parameters and variables for MRPP and other permutation procedures, LAD
!     and other regression commands, and other Blossom functions.
! LANGUAGE:
!     Fortran 90/95
! AUTHOR:
!     Jon D. Richards, Operations Research Analyst       jon_richards@usgs.gov
!     Fort Collins Science Center                     http://www.fort.usgs.gov
!     U.S. Geological Survey          -          Biological Resources Division
! MODIFICATION HISTORY:
!     written - JDR  15 Jan 1999
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
IMPLICIT NONE
SAVE
        ! Flag set upon building TEMP data set to tell Blossom there
        ! are missing values and that Blossom will need to screen for
        ! missing values in a manner consistent with the variables used
        ! and the analysis to be performed.
LOGICAL :: l_DoScreen4MsngVals
        ! Whether to consider the data as cyclic, periodic, circular
        ! in an MRPP command (analytical program does this).
LOGICAL :: l_DoArc
        ! User has specified a maximum cut-off value for distances that
        ! will be computed for a Permutation Procedure.  If a computed
        ! distance exceeds this, the value will be adjusted to this.
LOGICAL :: l_DoTrunc
        ! Default is to get random number seed from the system.
INTEGER, PARAMETER :: i_USE_SYS_RAND = -1
        ! Whether Blossom has commensurated the data.
LOGICAL :: l_IsCommens
        ! This will be the number of observations
        ! for the analytical program.  This number may be smaller
        ! than the iNCases, as some cases may have been thrown out
        ! if those cases contained variable(s) with missing value(s).
INTEGER :: i_NumRcd
        ! Number of Blocks determined by Blossom.
INTEGER :: i_NumBks
        ! Number of Groups determined by Blossom.
INTEGER :: i_NumGps
        ! Number of Grouping variable values determined by Blossom.
INTEGER :: i_NumGVVals
        ! Blossom has figured out the values for the grouping variable.
LOGICAL :: l_HasGVals
        ! Number of MRPP or MRSP variables.
INTEGER :: i_NumMVs
        ! Whether this is an "agreement" style of MRBP (obsolete).
LOGICAL :: l_IsAgBP
        ! Whether Blossom has aligned the data in MRBP.
LOGICAL :: l_IsAligned
        ! Whether this is an "mrbp" style of MRBP.
LOGICAL :: l_IsBPBP
        ! The program type we are working on: MRPP, EMRPP, MRBP or PTMP.
INTEGER :: i_ProgType
        ! Whether to do a Monte Carlo resampling to obtain PP P-Values or
        ! to use the default Pearson Type III estimation.
LOGICAL :: l_DoResample
        ! Whether to do an Exact GSECT coverage test.
LOGICAL :: l_DoEGSECT ! JDR  11 Feb 1999
        ! Whether to do an Exact MRBP analysis.
LOGICAL :: l_DoEMRBP  ! JDR  10 Feb 1999
        ! Whether to do an Exact MRSP - JDR  21 Jul 1999
LOGICAL :: l_DoEMRSP
        ! This is a COVerage test.
LOGICAL :: l_IsCover
        ! Random number seed for COVerage testing permutations. May be
        ! specified by user on Blossom command line, else it is
        ! obtained from the system.  May be set same as i_RandNumSeed.
INTEGER :: i_Seed
        ! This is a KSGF test.
LOGICAL :: l_IsKSGF
        ! This is a (LAD/QUANT) regression test.
LOGICAL :: l_IsLaPg
        ! This is a Sequence Procedure, an MRSP is to be run.
LOGICAL :: l_IsMRSP
        ! This is a (OLS) regression test.
LOGICAL :: l_IsOLS
        ! Whether this is an "paired" style of MRBP (obsolete).
LOGICAL :: l_IsPaBP
        ! This is a Sequence Procedure, an MRSP dataset is to be made.
LOGICAL :: l_IsSProc
        ! Number of Sequence Procedure variables.
INTEGER :: i_NumSPVars
        ! Find all the regression quantiles.
LOGICAL :: l_DoAllQuants
        ! The previous command was a regression, so the next command
        ! may be an hypothesis test of that regression.  Any number
        ! of hypotheses commands may be given refering back to the
        ! full model of the original regression.  Details of the
        ! original regression model and the latest hypothesis model
        ! are always at hand.
LOGICAL :: l_LadGoing
        ! Array for LAD commands: whether a command has an
        ! intercept term in the regression model.
LOGICAL :: la_HasIntercept(i_NUM_LAD_CMDS)
        ! Blossom has done a LAD /SAVE command to save
        ! certain regression results to a file.
LOGICAL :: l_DidLadSave
        ! Whether to apply Hotelling's commensuration adjustment to
        ! the data in grouped analyses.
LOGICAL :: l_DoHot
        ! Whether to do a LAD /SAVE after a regression analysis.
LOGICAL :: l_DoLadSave
        ! Use rank-score type analysis of regression testing.
LOGICAL :: l_DoRankScore  ! JDR   1 Mar 1999
        ! Number of variables in the full regression model and in
        ! the latest hypothesis regression model (if any).
LOGICAL :: l_TrueLad ! JDR  14 Jan 2000
        ! Whether to do a True LAD (or simply a 50th Quantile)
INTEGER :: ia_NumLaVars(i_NUM_LAD_CMDS)
        ! The position in cha_VarList of the dependent variable. So
        ! if the variable list is (va, vb, vc, vd) and the model is
        ! vc = constant + vd + va, then i_DepVarCmdPosn = 3.
INTEGER :: i_DepVarCmdPosn
        ! Character length of current Hypothesis Command line (if any).
INTEGER :: i_HypCmdLen
        ! Number of iterations to use for coverage test.
INTEGER :: i_Iter
        ! Character length of current LAD line.
INTEGER :: i_LadCmdLen
        ! LAD/Hypothesis program type: either a LAD or an Hypothesis.
        ! This variable is often used as an index to LAD/HYP arrays.
INTEGER :: i_LaHy
        ! LAD/Hypothesis program type: either a LAD or an Hypothesis.
INTEGER :: i_LaPgType
        ! Do a test (of regression model).
LOGICAL :: l_DoLadTest
        ! Number of permutations to perform for testing LAD/QUANT/OLS
        ! regression model.
INTEGER :: i_NumPermut
        ! Random number seed for LAD/QUANT/OLS regression testing
        ! permutations. May be specified by user on Blossom command
        ! line, else it is obtained from the system.
INTEGER :: i_RandNumSeed
        ! Blossom has a Hypothesis Blossom Command Line (and
        ! subsequently all pertenant info about that command).
LOGICAL :: l_HasHypCmdLine
        ! Blossom has a LAD Blossom Command Line (and subsequently all
        ! pertenant info about that command). A LAD Command information
        ! is saved in case the following command is an Hypothosis
        ! Command and then Blossom will need the LAD Command's info.
LOGICAL :: l_HasLadCmdLine
        ! Blossom has a valid LAD /SAVE Filename to utilize.
LOGICAL :: l_HasLADSavFilNam
        ! Blossom has a quantile value to use for a Quantile regression.
LOGICAL :: l_HasQuant
        ! Hold the user input image of the quantile specification here.
CHARACTER (LEN=i_MAX_ELEM_LEN) :: ch_QuantImage
        ! Array to indicate whether or not a case from data set is in the
        ! FULL model of a regression.  If it is, we want to include it
        ! in the REDUCED model, else we simply skip it in the REDUCED model.
LOGICAL, ALLOCATABLE :: la_IncludeCase(:) ! (1:i_NumObs)
! User can turn double permutations on and off for LAD/QUANT reduced model
! reqression tests
LOGICAL :: l_DoublePermutation

LOGICAL :: l_NewUse
        ! Blossom command specified a Sequence Procedure Variable.
LOGICAL :: l_HasSPVar
! MEDQ stuff
        ! Whether to do an rmedquant: calculate r-way (multivariate)
        ! medians and/or quantiles. - JDR  14 Feb 1999
LOGICAL :: l_DoMedQ
! Permutation Procedure C-Form weighting strings and codes:
CHARACTER (LEN=*), PARAMETER :: ch_C_FORM_1 = "n(I)/sum(n(I))"
CHARACTER (LEN=*), PARAMETER :: ch_C_FORM_2 = "(n(I)-1)/sum(n(I)-1)"
CHARACTER (LEN=*), PARAMETER :: ch_C_FORM_3 = "1/sum(1)"
CHARACTER (LEN=*), PARAMETER :: ch_C_FORM_4 =                                 &
                                 "(n(I)*(n(I)-1))/sum(n(I)*(n(I)-1))"
INTEGER, PARAMETER :: i_C_FORM_1 = 1
INTEGER, PARAMETER :: i_C_FORM_2 = 2
INTEGER, PARAMETER :: i_C_FORM_3 = 3
INTEGER, PARAMETER :: i_C_FORM_4 = 4
INTEGER,          PARAMETER :: i_DEFAULT_C_FORM   = i_C_FORM_1
REAL (KIND(0D0)), PARAMETER :: d_DEFAULT_V_DIST   = 1.0D0
REAL (KIND(0D0)), PARAMETER :: d_DEFLT_NO_TRUNC   = 0.0D0
REAL (KIND(0D0)), PARAMETER :: d_DEFAULT_NO_ARC   = 0.0D0
REAL (KIND(0D0)), PARAMETER :: d_DEFAULT_ARC_DIST = 1.0D0
! Program type codes:
INTEGER, PARAMETER :: i_PROG_TYPE_MRPP  = 1  ! For i_ProgType
INTEGER, PARAMETER :: i_PROG_TYPE_EMRPP = 2  ! For i_ProgType
INTEGER, PARAMETER :: i_PROG_TYPE_MRBP  = 3  ! For i_ProgType
INTEGER, PARAMETER :: i_PROG_TYPE_PTMP  = 4  ! For i_ProgType
INTEGER, PARAMETER :: i_PROG_TYPE_LAD1  = 1  ! For i_LaPgType
INTEGER, PARAMETER :: i_PROG_TYPE_HYP   = 2  ! For i_LaPgType
INTEGER, PARAMETER :: i_PROG_TYPE_ALLQ  = 5  ! For i_LaPgType
INTEGER, PARAMETER :: i_PROG_TYPE_MEDQ  = 6  ! For Medq
        ! Flag used to set as indicator that group is an excess group.
INTEGER, PARAMETER :: i_XS_FLAG            = -1
        ! Maximum number of dependent variables for LAD/HYP/OLS regression
INTEGER, PARAMETER :: i_MAX_NUM_INDEP_VARS = i_MAX_N_VARS - 1
        ! Maximum number of variables to drop from full to reduced
        ! regression model for hypothesis testing.
INTEGER, PARAMETER :: i_MAX_POS_2DROP = i_MAX_NUM_INDEP_VARS - 1
        ! Use all grouping variable values in Permutation Procedure
        ! analysis. Implied if user does not specify values.
INTEGER, PARAMETER :: i_USE_ALL_GVV        = 1
        ! Use grouping variable values between some minimum and maximum
        ! values in Permutation Procedure analysis.
        ! User specifies these on Blossom command line.
INTEGER, PARAMETER :: i_USE_RANGE_GVV      = 2
        ! Use list of grouping variable values in Permutation Procedure
        ! analysis. User specifies this on Blossom command line.
INTEGER, PARAMETER :: i_USE_LIST_GVV       = 3
        ! Do Hotelling commensuration flag.
INTEGER, PARAMETER :: i_DO_HOTELLING       = 1
        ! Matrix singular error. Scientific Subroutine Library, SSL2,
        !   returns 20000 if we try to invert a singular matrix.
INTEGER, PARAMETER :: i_MAT_SINGULARITY    = 20000
        ! Minimum number of observations in an MRPP.
INTEGER, PARAMETER :: i_MIN_MRPP_OBS       = 6
        ! Minimum number of groups to do an MRPP.
INTEGER, PARAMETER :: i_MIN_NUM_GPS_MRPP   = 2
        ! Minimum group size for MRPP.
INTEGER, PARAMETER :: i_MIN_GRP_SIZE_MRPP  = 4
        ! Minimum number of observations in an Exact MRPP.
INTEGER, PARAMETER :: i_MIN_EMRPP_OBS      = 3
        ! Minimum number of groups to do an Exact MRPP.
INTEGER, PARAMETER :: i_MIN_NUM_GPS_EMRPP  = 2
        ! Minimum group size for Exact MRPP.
INTEGER, PARAMETER :: i_MIN_GRP_SIZE_EMRPP = 2
        ! Minimum number of groups to do an MRBP.
INTEGER, PARAMETER :: i_MIN_NUM_GPS_MRBP   = 2
        ! Minimum number of groups to do an MRBP.
INTEGER, PARAMETER :: i_MIN_NUM_BKS_MRBP   = 2
        ! Minimum number of blocks in an Exact MRBP.
INTEGER, PARAMETER :: i_MIN_EMRBP_BLOCKS   = 2
        ! Maximum number of blocks in an Exact MRBP.
INTEGER, PARAMETER :: i_MAX_EMRBP_BLOCKS   = 9
        ! Minimum number of cases to do a PTMP (need at least 3 cases)
INTEGER, PARAMETER :: i_MIN_NUM_PTMP_CASES = 3
        ! If i_NumCases less than or equal to this, do an exact PTMP
INTEGER, PARAMETER :: i_NUM2DO_EPTMP       = 20
        ! Minimum number of observations in an MRSP.
INTEGER, PARAMETER :: i_MIN_MRSP_OBS       = 6
        ! Minimum number of observations in an Exact MRSP.
INTEGER, PARAMETER :: i_MIN_EMRSP_OBS      = 2
        ! Considered a LARGE Exact MRSP
INTEGER, PARAMETER :: i_LARGE_EMRSP_DATA   = 9
INTEGER, PARAMETER :: i_HUGE_EMRSP_DATA    = 11
        ! Minimum number of groups in a Cover Analysis.
INTEGER, PARAMETER :: i_MIN_NUM_GPS_COV    = 2
        ! Minimum group size for COV.
INTEGER, PARAMETER :: i_MIN_GRP_SIZE_COV   = 2
        ! Considered a LARGE Exact GSECT
INTEGER, PARAMETER :: i_LARGE_EGSECT_DATA  = 10
INTEGER, PARAMETER :: i_HUGE_EGSECT_DATA   = 12
! LAD/QUANT regression exit codes:
INTEGER, PARAMETER :: i_NON_UNIQUE_SOLN = 0 ! non-unique but optimal solution.
INTEGER, PARAMETER :: i_SUCCESSFUL_SOLN = 1 ! successful, unique solution.
INTEGER, PARAMETER :: i_ROUNDING_ERRORS = 2 ! rounding errors occured.
INTEGER, PARAMETER :: i_2MANY_SOLUTIONS = 7 ! Too many solutions in AllQuant
! Regression testing codes:
INTEGER, PARAMETER :: i_NO_TEST            =  0
INTEGER, PARAMETER :: i_DO_FULL_TEST       =  1
INTEGER, PARAMETER :: i_DO_NOT_FULL_TEST   =  2
INTEGER, PARAMETER :: i_DO_RANK_SCORE_TEST =  3
        ! Use this for a Grouping Variable Name if we need one for
        ! processing by a subroutine, but where we have no grouping
        ! variable.
CHARACTER (LEN=*), PARAMETER :: ch_NOT_GVAR_NAME = "111"
  ! The following are the maximum dimensions of the analytical programs
  ! on the PC.  They are checked in the runXXXX programs.
  ! maxdanpg dimension(number of analytical programs,number of parameters)
  ! Where the row index is:
  !   1 = MRPP
  !   2 = EMRPP
  !   3 = PTMP
  !   4 = MRBP
  !   5 = AGREE (MRBP for more groups)
  !   6 = (PAIRED) (MRBP for more blocks)(obsolete)
  !   7 = LAD
  !   8 = (LAD's AGREE) (obsolete)
  !   9 = (LAD's PTMP)  (obsolete)
  !  10 = GSECT1 coverage test
  !  11 = MRSP
  ! and the column index is the max for:
  !   1 = number of observations (cases)
  !   2 = number of groups
  !   3 = number of response variables (i_MAX_N_VARS)
  !   4 = number of blocks
  ! IMPORTANT. These are maximum dimensions of analytical programs
  !  rows are for:
  !    MRPP, EMRPP, PTMP, MRBP, AGREE, (PAIRED), LAD,
  !    (LAD's AGREE),  (LAD's PTMP), GSECT1, MRSP, KSGF,
  !
  !   (-1 means that this need not be checked).
INTEGER, PARAMETER :: i_2PWR31M1 = 2147483647
! INTEGER, PARAMETER :: i_2POWER30 = 1073741824
INTEGER, PARAMETER :: i_BIG_LIMIT = i_2PWR31M1
INTEGER, PARAMETER :: i_NA = -1
INTEGER, PARAMETER :: ia_MAX_DIM_AN_PROG(20,4) = RESHAPE(                     &
(/                                                                            &
i_BIG_LIMIT, i_BIG_LIMIT, i_BIG_LIMIT, i_BIG_LIMIT, i_BIG_LIMIT, i_BIG_LIMIT, &
i_BIG_LIMIT,        i_NA,        i_NA, i_BIG_LIMIT, i_BIG_LIMIT, i_BIG_LIMIT, &
     150000,      400000,          10,        i_NA,        i_NA,        i_NA, &
       i_NA,        i_NA,                                                     &
i_BIG_LIMIT, i_BIG_LIMIT,        i_NA, i_BIG_LIMIT, i_BIG_LIMIT,           2, &
       i_NA,        i_NA,        i_NA, i_BIG_LIMIT,        i_NA,        i_NA, &
       i_NA,          20,          25,        i_NA,        i_NA,        i_NA, &
       i_NA,        i_NA,                                                     &
i_MAX_MR_VARS,i_MAX_MR_VARS,i_NA,i_MAX_MR_VARS, i_MAX_MR_VARS, i_MAX_MR_VARS, &
i_MAX_LAD_VARS,     i_NA,        i_NA,           1,   i_MAX_SP_VARS,    i_NA, &
       i_NA,          20,        i_NA,        i_NA,        i_NA,        i_NA, &
       i_NA,        i_NA,                                                     &
       i_NA,        i_NA,        i_NA, i_BIG_LIMIT, i_BIG_LIMIT, i_BIG_LIMIT  &
/), (/20,4/), PAD=(/i_NA/) )

! The indexes in the above matrix are parameters:
INTEGER, PARAMETER :: i_AGREE_LIMIT     = 5
INTEGER, PARAMETER :: i_EMRPP_LIMIT     = 2
INTEGER, PARAMETER :: i_GSECT1_LIMIT    = 10
INTEGER, PARAMETER :: i_KSGF_LIMIT      = 12
INTEGER, PARAMETER :: i_LAD_AGREE_LIMIT = 8
INTEGER, PARAMETER :: i_LAD_LIMIT       = 7
INTEGER, PARAMETER :: i_LAD_PTMP_LIMIT  = 9
INTEGER, PARAMETER :: i_MRBP_LIMIT      = 4
INTEGER, PARAMETER :: i_MRPP_LIMIT      = 1
INTEGER, PARAMETER :: i_MRSP_LIMIT      = 11
INTEGER, PARAMETER :: i_PAIRED_LIMIT    = 6
INTEGER, PARAMETER :: i_PTMP_LIMIT      = 3
INTEGER, PARAMETER :: i_BLK_LIMIT = 4
INTEGER, PARAMETER :: i_GRP_LIMIT = 2
INTEGER, PARAMETER :: i_OBS_LIMIT = 1
INTEGER, PARAMETER :: i_VAR_LIMIT = 3
        ! The number of units in a complete cycle, period, circle for
        ! which we will want to compute ARC distances in Permutation
        ! Procedures.  e.g., 24 for 24-hour days, where we want the
        ! distance from 11p.m. to 1a.m. to be 2, not 22. Or from 355 degrees
        ! to 5 degrees to be 10, not 350.  These should be in the units
        ! in which the data were collected.
REAL (KIND(0D0)) :: d_ArcIntv
        ! Quantile value for which we are to compute a regression.
REAL (KIND(0D0)) :: d_Theta
        ! User specified value beyond which Permutation Procedure
        ! distances will not exceed.  If the compued distance is larger
        ! than this, it is truncated to this value.
REAL (KIND(0D0)) :: d_Trunc
        ! List of independent regression model variable names.
! CHARACTER (LEN=i_MAX_LEN_VAR_NAME):: cha_LAVarList(i_NUM_LAD_CMDS,i_MAX_LAD_VARS)
CHARACTER (LEN=i_MAX_LEN_VAR_NAME), ALLOCATABLE :: cha_LAVarList(:,:)
        ! List of Permutation Procedure variable names in command, not
        ! including any grouping or blocking type variables.
! CHARACTER (LEN=i_MAX_LEN_VAR_NAME) :: cha_MRVarList(i_MAX_N_VARS)
CHARACTER (LEN=i_MAX_LEN_VAR_NAME), ALLOCATABLE :: cha_MRVarList(:)
        ! Cover variable name.
CHARACTER (LEN=i_MAX_LEN_VAR_NAME) :: ch_CovVarNam
        ! Regression model dependent variable name.
CHARACTER (LEN=i_MAX_LEN_VAR_NAME) :: ch_DepVarNam
        ! Grouping variable name.
CHARACTER (LEN=i_MAX_LEN_VAR_NAME) :: ch_GrpVarNam
!x CHARACTER (LEN=i_MAX_LEN_VAR_NAME) :: ch_ResVarN
        ! Sequence Procedure variable name.
CHARACTER (LEN=i_MAX_LEN_VAR_NAME) :: ch_SPVarName
        !xxx File name to use for LAD /SAVE file.
!xxx CHARACTER (LEN=i_MAX_LEN_VAR_NAME) :: ch_LADSavFilNam
        ! Variable to store the entire Hypothesis Command from the
        ! Blossom command line.
CHARACTER (LEN=i_MAX_CMD_LENGTH) :: ch_HypCmdLine
        ! Variable to store the entire LAD Command from the
        ! Blossom command line.
CHARACTER (LEN=i_MAX_CMD_LENGTH) :: ch_LadCmdLine
END MODULE mrolamod

!     ******************************************************************
!     comndmod.f90
!     Author: Jon D. Richards    U.S. Geological Survey
!     Last change: JDR 4/10/2006 1:51:50 PM
!     ******************************************************************
!#/WINTERACTER/ ! This Comment means it is for Windows (WINTERACTER) version only.             !#/WINTERACTER/
! This Comment means it is for Console (CON) version only.                     !#/CON/
!  LF_OPTIONIS <File: "comndmod.f90: lf_OptionIs">
!=============================================================================!
MODULE comndmod
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     MODULE comndmod
! DESCRIPTION:
!     Command, swap, and option codes and procedures.
!     Command abbreviations, command-type codes, SWAP command-codes,
!     option codes.
!     SWAP Codes indicate internaly in the SWAP file which
!     analysis is in progress and thus which data are swapped out.
!     Derived from old Blossom block data common blocks:
!        /cmdtyp/ & /cmdabr/ -JDR  14 Jan 1999
! LANGUAGE:
!     Fortran 90/95
! AUTHOR:
!     Jon D. Richards, Operations Research Analyst       jon_richards@usgs.gov
!     Fort Collins Science Center                     http://www.fort.usgs.gov
!     U.S. Geological Survey          -          Biological Resources Division
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
IMPLICIT NONE
SAVE
        ! Command Abbreviations - Look at first 2 chars only for commands.
        ! (Someday we may need to go to more than 2).
! CHARACTER (LEN=*), PARAMETER :: ch_CHGDIR_CMD = "CD"
CHARACTER (LEN=*), PARAMETER :: ch_COVER_CMD  = "CO"
CHARACTER (LEN=*), PARAMETER :: ch_CHGDIR_CMD = "CD"
CHARACTER (LEN=*), PARAMETER :: ch_DATE_CMD   = "DA"
CHARACTER (LEN=*), PARAMETER :: ch_ECHO_CMD   = "EC"
CHARACTER (LEN=*), PARAMETER :: ch_HELP_CMD   = "HE"
CHARACTER (LEN=*), PARAMETER :: ch_HYP_CMD    = "HY"
CHARACTER (LEN=*), PARAMETER :: ch_LAD_CMD    = "LA"
CHARACTER (LEN=*), PARAMETER :: ch_MEDQ_CMD   = "ME"
CHARACTER (LEN=*), PARAMETER :: ch_MRPP_CMD   = "MR"
CHARACTER (LEN=*), PARAMETER :: ch_NOTE_CMD   = "NO"
CHARACTER (LEN=*), PARAMETER :: ch_OLS_CMD    = "OL"
CHARACTER (LEN=*), PARAMETER :: ch_OUTPUT_CMD = "OU" ! CHARACTER (LEN=*), PARAMETER :: ch_OUTPATH_CMD= "OU"
CHARACTER (LEN=*), PARAMETER :: ch_QUIT_CMD   = "QU"
CHARACTER (LEN=*), PARAMETER :: ch_RANDOM_CMD = "RA"
CHARACTER (LEN=*), PARAMETER :: ch_SAVE_CMD   = "SA"
CHARACTER (LEN=*), PARAMETER :: ch_SHELL_CMD  = "SH"
CHARACTER (LEN=*), PARAMETER :: ch_MRSP_CMD   = "SP"
CHARACTER (LEN=*), PARAMETER :: ch_STATUS_CMD = "ST"
CHARACTER (LEN=*), PARAMETER :: ch_SUBMIT_CMD = "SU"
CHARACTER (LEN=*), PARAMETER :: ch_TITLE_CMD  = "TI"
CHARACTER (LEN=*), PARAMETER :: ch_USE_CMD    = "US"
        ! Command Types
INTEGER :: i_CmdType
! INTEGER, PARAMETER :: i_CMD_CHDIR  =  18
INTEGER, PARAMETER :: i_CMD_CHDIR  =  18
INTEGER, PARAMETER :: i_CMD_COVER  =  13
INTEGER, PARAMETER :: i_CMD_DATE   =  12
INTEGER, PARAMETER :: i_CMD_ECHO   =  14
INTEGER, PARAMETER :: i_CMD_HELP   =   8
INTEGER, PARAMETER :: i_CMD_HYP    =  11
INTEGER, PARAMETER :: i_CMD_LAD    =   1
INTEGER, PARAMETER :: i_CMD_MEDQ   =  20
INTEGER, PARAMETER :: i_CMD_MRPP   =   0
INTEGER, PARAMETER :: i_CMD_MRSP   =  15
INTEGER, PARAMETER :: i_CMD_NONE   = 999
INTEGER, PARAMETER :: i_CMD_NOTE   =  21
INTEGER, PARAMETER :: i_CMD_OLS    =  19
INTEGER, PARAMETER :: i_CMD_OUTPUT =   9
INTEGER, PARAMETER :: i_CMD_QUIT   =   3
INTEGER, PARAMETER :: i_CMD_RANDOM =  22
INTEGER, PARAMETER :: i_CMD_SAVE   =   6
INTEGER, PARAMETER :: i_CMD_SHELL  =   2
INTEGER, PARAMETER :: i_CMD_STATUS =   5
INTEGER, PARAMETER :: i_CMD_SUBMIT =  10
INTEGER, PARAMETER :: i_CMD_TITLE  =   4
INTEGER, PARAMETER :: i_CMD_UNDEF  =  -1
INTEGER, PARAMETER :: i_CMD_USE    =   7
!         ! SWAP command codes.
! CHARACTER (LEN=*), PARAMETER :: ch_SWAP_CMD_COV = "COV"
! CHARACTER (LEN=*), PARAMETER :: ch_SWAP_CMD_LAD = "LAD"
! CHARACTER (LEN=*), PARAMETER :: ch_SWAP_CMD_MRP = "MRP"
! CHARACTER (LEN=*), PARAMETER :: ch_SWAP_CMD_NON = "NON"
! CHARACTER (LEN=*), PARAMETER :: ch_SWAP_CMD_OLS = "OLS"
! CHARACTER (LEN=*), PARAMETER :: ch_SWAP_CMD_SEQ = "SEQ"
        ! Options to Blossom commands.
INTEGER, PARAMETER :: i_OPT_ARC         =  1
INTEGER, PARAMETER :: i_OPT_C_FORM      =  2
INTEGER, PARAMETER :: i_OPT_DIST_EXP_V  =  3
INTEGER, PARAMETER :: i_OPT_DEFAULT     =  4
INTEGER, PARAMETER :: i_OPT_ECHO_DATA   =  5
INTEGER, PARAMETER :: i_OPT_ECHO_OUTPUT =  6
INTEGER, PARAMETER :: i_OPT_EXACT       =  7
INTEGER, PARAMETER :: i_OPT_EXCESS_GRP  =  8
INTEGER, PARAMETER :: i_OPT_HOTELLING   =  9
INTEGER, PARAMETER :: i_OPT_ITERATIONS  = 10
INTEGER, PARAMETER :: i_OPT_LEVERAGE    = 11
INTEGER, PARAMETER :: i_OPT_MT          = 12
INTEGER, PARAMETER :: i_OPT_NO_ALIGN    = 13
INTEGER, PARAMETER :: i_OPT_NO_COMMENS  = 14
INTEGER, PARAMETER :: i_OPT_NUM_PERMS   = 15
INTEGER, PARAMETER :: i_OPT_OFF         = 16
INTEGER, PARAMETER :: i_OPT_ON          = 17
INTEGER, PARAMETER :: i_OPT_PAIRED      = 18
INTEGER, PARAMETER :: i_OPT_QUANTILE    = 19
INTEGER, PARAMETER :: i_OPT_RANK_SCORE  = 20
INTEGER, PARAMETER :: i_OPT_SAVE        = 21
INTEGER, PARAMETER :: i_OPT_SEED        = 22
INTEGER, PARAMETER :: i_OPT_TEST        = 23
INTEGER, PARAMETER :: i_OPT_TRUNCATE    = 25
INTEGER, PARAMETER :: i_OPT_DOUBLE_PERM = 26
INTEGER, PARAMETER :: i_OPT_SAVETEST    = 27
INTEGER, PARAMETER :: i_OPT_TERSE       = 28
INTEGER, PARAMETER :: i_OPT_VERBOSE     = 29
INTEGER, PARAMETER :: i_OPT_PATH        = 30
! Regression types
INTEGER            :: i_RegressionMode
INTEGER, PARAMETER ::       UNASSIGNED =  0
INTEGER, PARAMETER ::       SIMPLE_LAD =  1
INTEGER, PARAMETER ::     SIMPLE_QUANT =  2
INTEGER, PARAMETER ::         LAD_TEST =  3
INTEGER, PARAMETER ::    HYP_LAD_QUANT =  4
INTEGER, PARAMETER ::   HYP_DELETE_QM1 =  5
INTEGER, PARAMETER ::    HYP_RANKSCORE =  6
INTEGER, PARAMETER :: HYP_RANKSCORE_DP =  7
INTEGER, PARAMETER ::  HYP_DOUBLE_PERM =  8
INTEGER, PARAMETER ::        QUANT_ALL =  9
INTEGER, PARAMETER ::              OLS = 10
INTEGER, PARAMETER ::         OLS_TEST = 11
INTEGER, PARAMETER ::          HYP_OLS = 12
INTEGER, PARAMETER ::       HYP_OLS_DP = 13
INTEGER, PARAMETER ::   HYP_QM1_AND_DP = 14
CONTAINS
!==============================================================================
FUNCTION lf_OptionIs(ch_Command, i_Option) RESULT(l_Result)
!___Dummy Arguments:
CHARACTER (LEN=*), INTENT(IN) :: ch_Command
INTEGER,           INTENT(IN) :: i_Option
!___Function Return Result:
LOGICAL :: l_Result
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     LOGICAL FUNCTION lf_OptionIs
! DESCRIPTION:
!     Test if current element in the command stack is a specified option.
! AUTHOR:
!     Jon D. Richards, Operations Research Analyst       jon_richards@usgs.gov
!     Fort Collins Science Center                     http://www.fort.usgs.gov
!     U.S. Geological Survey          -          Biological Resources Division
! INVOKED BY:
!     cooption     <File: "docover.f90: cooption">    module
!     domrpp       <File: "domrpp.f90: domrpp">       module
!     getquantlist <File: "domedq.f90: getquantlist"> module
!     laoption     <File: "dolad.f90: laoption">      module
!     mqoption     <File: "domedq.f90: mqoption">     module
!     mroption     <File: "domrpp.f90: mroption">     module
!     spoption     <File: "domrsp.f90: spoption">     module
! INVOKES:
!     -
! FILES:
!     -
! DEVICES:
!     -
! MODIFICATION HISTORY:
!   CODE HISTORY:
!     Written - JDR   9 Aug 1999
!     Mod - Make "/ITER" synonymous with "/NPERM".  Rearrange to put
!           most "expected" forms of the options first. - JDR  18 May 2000
!   COMMENT HISTORY:
!     Written - JDR   9 Aug 1999
!     Updated - JDR  10 Aug 1999
!     Updated - JDR  18 May 2000
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!___Local Variables:
!__(none)__
!___Executable Statements:
   l_Result = .FALSE.

SELECT CASE (i_Option)
   CASE (i_OPT_ARC)
      IF (ch_Command == "ARC"    .OR.  &
          ch_Command == "ARCO"   .OR.  &
          ch_Command == "BOGEN") THEN
         l_Result = .TRUE.
      END IF
   CASE (i_OPT_C_FORM)
      IF (ch_Command == "C"      .OR.  &
          ch_Command == "CF"     .OR.  &
          ch_Command == "CFORM") THEN
         l_Result = .TRUE.
      END IF
   CASE (i_OPT_DEFAULT)
      IF (ch_Command == "DEF"      .OR.  &
          ch_Command == "DEFAULT") THEN
         l_Result = .TRUE.
      END IF
   CASE (i_OPT_DIST_EXP_V)
      IF (ch_Command == "V"      .OR.  &
          ch_Command == "DIST"   .OR.  &
          ch_Command == "EP"     .OR.  &
          ch_Command == "EXP"    .OR.  &
          ch_Command == "EXPON") THEN
         l_Result = .TRUE.
      END IF
   CASE (i_OPT_ECHO_DATA)
      IF (ch_Command == "DATA"   .OR.  &
          ch_Command == "IN"     .OR.  &
          ch_Command == "INPUT") THEN
         l_Result = .TRUE.
      END IF
   CASE (i_OPT_ECHO_OUTPUT)
      IF (ch_Command == "OUTPUT"  .OR.  &
          ch_Command == "OUT"     .OR.  &
          ch_Command == "RESULTS" .OR.  &
          ch_Command == "RESULT") THEN
         l_Result = .TRUE.
      END IF
   CASE (i_OPT_EXACT)
      IF (ch_Command == "EXACT"   .OR.  &
          ch_Command == "ESATTO"  .OR.  &
          ch_Command == "EXACTA"  .OR.  &
          ch_Command == "EXACTE"  .OR.  &
          ch_Command == "EXACTO"  .OR.  &
          ch_Command == "EXATA"   .OR.  &
          ch_Command == "EXATO"   .OR.  &
          ch_Command == "GENAUE"  .OR.  &
          ch_Command == "XA") THEN
         l_Result = .TRUE.
      END IF
   CASE (i_OPT_EXCESS_GRP)
      IF (ch_Command == "EXCESS"     .OR.  &
          ch_Command == "EXCES"      .OR.  &
          ch_Command == "EXCESO"     .OR.  &
          ch_Command == "EXCESSO"    .OR.  &
          ch_Command == "UBERFLUSS"  .OR.  &
          ch_Command == "XS") THEN
         l_Result = .TRUE.
      END IF
   CASE (i_OPT_HOTELLING)
      IF (ch_Command == "HOT"    .OR.  &
          ch_Command == "HOTEL"  .OR.  &
          ch_Command == "HOTELLING") THEN
         l_Result = .TRUE.
      END IF
   CASE (i_OPT_ITERATIONS)  ! Let us make syn with perms
      IF (ch_Command == "NPERM"          .OR.  &
          ch_Command == "ITER"           .OR.  &
          ch_Command == "RESAMP"         .OR.  &
          ch_Command == "SAMP"           .OR.  &
          ch_Command == "NPERMS"         .OR.  &
          ch_Command == "NUMPERM"        .OR.  &
          ch_Command == "NUMPERMS"       .OR.  &
          ch_Command == "PERM"           .OR.  &
          ch_Command == "PERMS"          .OR.  &
          ch_Command == "RESAMPLE"       .OR.  &
          ch_Command == "SAMPLE"         .OR.  &
          ch_Command == "SAMPLES"        .OR.  &
          ch_Command == "IT"             .OR.  &
          ch_Command == "ITERACIONES"    .OR.  &
          ch_Command == "ITERACOES"      .OR.  &
          ch_Command == "ITERATIONEN"    .OR.  &
          ch_Command == "ITERATIONS"     .OR.  &
          ch_Command == "NP"             .OR.  &
          ch_Command == "NPER"           .OR.  &
          ch_Command == "NUMP"           .OR.  &
          ch_Command == "PERMUTACIONES"  .OR.  &
          ch_Command == "PERMUTACOES"    .OR.  &
          ch_Command == "PERMUTATIONEN"  .OR.  &
          ch_Command == "PERMUTATIONS"   .OR.  &
          ch_Command == "PERMUTAZIONI"   .OR.  &
          ch_Command == "PERMUTE"        .OR.  &
          ch_Command == "PERMUTES"       .OR.  &
          ch_Command == "RESAMPLES"      .OR.  &
          ch_Command == "RIPETIZIONI") THEN
         l_Result = .TRUE.
      END IF
 CASE (i_OPT_NO_ALIGN)
      IF (ch_Command == "NOALIGN"       .OR.  &
          ch_Command == "NOAL"          .OR.  &
          ch_Command == "NAOALINHE"     .OR.  &
          ch_Command == "NICHTRICHTEN"  .OR.  &
          ch_Command == "NOALINEE"      .OR.  &
          ch_Command == "NONALIGNEZ"    .OR.  &
          ch_Command == "NONALLINEARE"  .OR.  &
          ch_Command == "PASALIGNEZ") THEN
         l_Result = .TRUE.
      END IF
   CASE (i_OPT_NO_COMMENS)
      IF (ch_Command == "NOCOMM"            .OR.  &
          ch_Command == "NOCOM"             .OR.  &
          ch_Command == "NAOPROPORCIONAL"   .OR.  &
          ch_Command == "NICHTANGEMESSEN"   .OR.  &
          ch_Command == "NOCOMENSURADO"     .OR.  &
          ch_Command == "NOCOMMENSURATION"  .OR.  &
          ch_Command == "NONPROPORZIONATO"  .OR.  &
          ch_Command == "PASPROPORTIONNE")  THEN
         l_Result = .TRUE.
      END IF
   CASE (i_OPT_NUM_PERMS)
      IF (ch_Command == "NPERM"          .OR.  &
          ch_Command == "ITER"           .OR.  &
          ch_Command == "RESAMP"         .OR.  &
          ch_Command == "SAMP"           .OR.  &
          ch_Command == "NPERMS"         .OR.  &
          ch_Command == "NUMPERM"        .OR.  &
          ch_Command == "NUMPERMS"       .OR.  &
          ch_Command == "PERM"           .OR.  &
          ch_Command == "PERMS"          .OR.  &
          ch_Command == "RESAMPLE"       .OR.  &
          ch_Command == "SAMPLE"         .OR.  &
          ch_Command == "SAMPLES"        .OR.  &
          ch_Command == "IT"             .OR.  &
          ch_Command == "ITERACIONES"    .OR.  &
          ch_Command == "ITERACOES"      .OR.  &
          ch_Command == "ITERATIONEN"    .OR.  &
          ch_Command == "ITERATIONS"     .OR.  &
          ch_Command == "NP"             .OR.  &
          ch_Command == "NPER"           .OR.  &
          ch_Command == "NUMP"           .OR.  &
          ch_Command == "PERMUTACIONES"  .OR.  &
          ch_Command == "PERMUTACOES"    .OR.  &
          ch_Command == "PERMUTATIONEN"  .OR.  &
          ch_Command == "PERMUTATIONS"   .OR.  &
          ch_Command == "PERMUTAZIONI"   .OR.  &
          ch_Command == "PERMUTE"        .OR.  &
          ch_Command == "PERMUTES"       .OR.  &
          ch_Command == "RESAMPLES"      .OR.  &
          ch_Command == "RIPETIZIONI") THEN
         l_Result = .TRUE.
      END IF
   CASE (i_OPT_DOUBLE_PERM)
      IF (ch_Command == "DOUBLEPERM"     .OR.  &
          ch_Command == "DOUBLE"         .OR.  &
          ch_Command == "DOUBLEP"        .OR.  &
          ch_Command == "DOUBLEPRM"      .OR.  &
          ch_Command == "DP"             .OR.  &
          ch_Command == "DBLP"           .OR.  &
          ch_Command == "DBLPRM"         .OR.  &
          ch_Command == "DBLPERM"        .OR.  &
          ch_Command == "DBLEP"          .OR.  &
          ch_Command == "DBLEPRM"        .OR.  &
          ch_Command == "DBLEPERM"       .OR.  &
          ch_Command == "DPERM"          .OR.  &
          ch_Command == "DOUBPERM"       .OR.  &
          ch_Command == "DUB"            .OR.  &
          ch_Command == "DUBP"           .OR.  &
          ch_Command == "DUBPERM"        .OR.  &
          ch_Command == "DUBPERMUTATION" .OR.  &
          ch_Command == "DOUBLEPERMUTATION") THEN
         l_Result = .TRUE.
      END IF
   CASE (i_OPT_OFF)
      IF (ch_Command == "OFF"    .OR.  &
          ch_Command == "NO"     .OR.  &
          ch_Command == "FALSE") THEN
         l_Result = .TRUE.
      END IF
   CASE (i_OPT_ON)
      IF (ch_Command == "ON"    .OR.  &
          ch_Command == "YES"   .OR.  &
          ch_Command == "TRUE") THEN
         l_Result = .TRUE.
      END IF
   CASE (i_OPT_PAIRED)
      IF (ch_Command == "PAIR"          .OR.  &
          ch_Command == "PAIRED"        .OR.  &
          ch_Command == "PAIRS"         .OR.  &
          ch_Command == "ACCOPPIATI"    .OR.  &
          ch_Command == "APPAREILLES"   .OR.  &
          ch_Command == "EMPAREJAN"     .OR.  &
          ch_Command == "EMPARELHADOS"  .OR.  &
          ch_Command == "ZUSAMMENGEPASST") THEN
         l_Result = .TRUE.
      END IF
   CASE (i_OPT_QUANTILE)
      IF (ch_Command == "QUANT"      .OR.  &
          ch_Command == "QUANTILE"   .OR.  &
          ch_Command == "QUANTILES"  .OR.  &
          ch_Command == "Q"          .OR.  &
          ch_Command == "QUAN"       .OR.  &
          ch_Command == "QUANS"      .OR.  &
          ch_Command == "QUANTS") THEN
         l_Result = .TRUE.
      END IF
   CASE (i_OPT_RANK_SCORE)
      IF (ch_Command == "RANK"        .OR.  &
          ch_Command == "RANKS"       .OR.  &
          ch_Command == "RANKSCORE"   .OR.  &
          ch_Command == "RS"          .OR.  &
          ch_Command == "RSCORE"      .OR.  &
          ch_Command == "SCORE"       .OR.  &
          ch_Command == "CONTAGEM"    .OR.  &
          ch_Command == "CUENTA"      .OR.  &
          ch_Command == "FILA"        .OR.  &
          ch_Command == "FILACUENTA"  .OR.  &
          ch_Command == "KERBE"       .OR.  &
          ch_Command == "ORDNEN"      .OR.  &
          ch_Command == "POINTS"      .OR.  &
          ch_Command == "RANGER"      .OR.  &
          ch_Command == "SEGNO") THEN
         l_Result = .TRUE.
      END IF
   CASE (i_OPT_SAVE)
      IF (ch_Command == "SAVE"       .OR.  &
          ch_Command == "SALVAR"     .OR.  &
          ch_Command == "SALVARE"    .OR.  &
          ch_Command == "SICHERN"    .OR.  &
          ch_Command == "CONSERVAR"  .OR.  &
          ch_Command == "SAUVEGARDER") THEN
         l_Result = .TRUE.
      END IF
   CASE (i_OPT_SAVETEST)
      IF (ch_Command == "SAVETEST") THEN
         l_Result = .TRUE.
      END IF
   CASE (i_OPT_SEED)
      IF (ch_Command == "SEED"         .OR.  &
          ch_Command == "RAND"         .OR.  &
          ch_Command == "RANDOM"       .OR.  &
          ch_Command == "RANDOMSEED"   .OR.  &
          ch_Command == "RANDOM_SEED"  .OR.  &
          ch_Command == "RANDSEED"     .OR.  &
          ch_Command == "GERMEN"       .OR.  &
          ch_Command == "GRAINE"       .OR.  &
          ch_Command == "SEME"         .OR.  &
          ch_Command == "SEMENTE"      .OR.  &
          ch_Command == "STARTWERT") THEN
         l_Result = .TRUE.
      END IF
   CASE (i_OPT_TEST)
      IF (ch_Command == "TEST"     .OR.  &
          ch_Command == "TES"      .OR.  &
          ch_Command == "ESSAI"    .OR.  &
          ch_Command == "PROVA"    .OR.  &
          ch_Command == "PRUEBA"   .OR.  &
          ch_Command == "PRUEBE"   .OR.  &
          ch_Command == "PRUFEN"   .OR.  &
          ch_Command == "TESTAMO"  .OR.  &
          ch_Command == "TESTEZ"   .OR.  &
          ch_Command == "VERIFICHIAMO") THEN
         l_Result = .TRUE.
      END IF
   CASE (i_OPT_TRUNCATE)
      IF (ch_Command == "TRUNC"   .OR.  &
          ch_Command == "TRUN"    .OR.  &
          ch_Command == "SUP"     .OR.  &
          ch_Command == "CUT"     .OR.  &
          ch_Command == "CUTOFF"  .OR.  &
          ch_Command == "TR") THEN
         l_Result = .TRUE.
      END IF
   CASE (i_OPT_PATH)
      IF (ch_Command == "PATH"      .OR. &
          ch_Command == "PATHNAME"  .OR. &
          ch_Command == "FOLDER"    .OR. &
          ch_Command == "DIRECTORY" .OR. &
          ch_Command == "FICHIER"   .OR. &
          ch_Command == "FILE") THEN
         l_Result = .TRUE.
      END IF
   CASE (i_OPT_TERSE)
      IF (ch_Command == "TERSE"      .OR.  &
          ch_Command == "SHORT"      .OR.  &
          ch_Command == "BRIEF"      .OR.  &
          ch_Command == "CONCISE"    .OR.  &
          ch_Command == "SUCCINCT"   .OR.  &
          ch_Command == "KURZ"       .OR.  &
          ch_Command == "KNAPP"      .OR.  &
          ch_Command == "SUCCINTO"   .OR.  &
          ch_Command == "SUCINTO"    .OR.  &
          ch_Command == "CONCIS"     .OR.  &
          ch_Command == "CONCISO"    .OR.  &
          ch_Command == "BREVE"      .OR.  &
          ch_Command == "COURT"      .OR.  &
          ch_Command == "EXPEDIENTE" .OR.  &
          ch_Command == "LACONIQUE") THEN
         l_Result = .TRUE.
      END IF
   CASE (i_OPT_VERBOSE)
      IF (ch_Command == "VERBOSE"   .OR.  &
          ch_Command == "LONG"      .OR.  &
          ch_Command == "DETAILED"  .OR.  &
          ch_Command == "DETALLADO" .OR.  &
          ch_Command == "VERBEUX"   .OR.  &
          ch_Command == "LONGUE"    .OR.  &
          ch_Command == "VERBOS"    .OR.  &
          ch_Command == "VERBOSO"   .OR.  &
          ch_Command == "WORTREICH" .OR.  &
          ch_Command == "PROLIXO"   .OR.  &
          ch_Command == "LARGO"     .OR.  &
          ch_Command == "LANG"      .OR.  &
          ch_Command == "LUNGO"     .OR.  &
          ch_Command == "LONGO"     .OR.  &
          ch_Command == "DETTAGLIATO") THEN
         l_Result = .TRUE.
      END IF
   CASE (i_OPT_MT)
      IF (ch_Command == "MT"       .OR.  &
          ch_Command == "MERSENNE" .OR.  &
          ch_Command == "TWISTER"  .OR.  &
          ch_Command == "TWIST"    .OR.  &
          ch_Command == "MER"      .OR.  &
          ch_Command == "MERS"     .OR.  &
          ch_Command == "TWISTOR") THEN
         l_Result = .TRUE.
      END IF
   CASE DEFAULT
     l_Result = .FALSE.
END SELECT
END FUNCTION lf_OptionIs
!==============================================================================
END MODULE comndmod




MODULE jonsmodule
!d USE debugmod                                                        ! debug!
! USE mrolamod, ONLY: i_MAT_SINGULARITY
!         ! ^-- Permutation, regression and other Blossom stat parameters.
IMPLICIT NONE
SAVE
!=============================================================================!
!     MODULE jonsmodule
! DESCRIPTION:
!     Contains some general routines I use frequently.
! LANGUAGE:
!     Fortran 90/95
! AUTHOR:
!     Jon D. Richards, Operations Research Analyst       jon_richards@usgs.gov
!     Fort Collins Science Center                     http://www.fort.usgs.gov
!     U.S. Geological Survey          -          Biological Resources Division
! MODIFICATION HISTORY:
!     Date Created:   28 Jun 1999
!=============================================================================!
!___Parameters:
        ! Error code. Often sent to Lahey's routine IOS_MSG
INTEGER :: i_CCode

        !   -= ERROR MESSAGES (for errhand) =-
CHARACTER (LEN=*), PARAMETER :: ch_EM00000 = &
      " "
CHARACTER (LEN=*), PARAMETER :: ch_EM00001 = &
      "$1$n ^-- Unrecognized command here in Blossom"
CHARACTER (LEN=*), PARAMETER :: ch_EM00002 = &
      "Problem reading ""$1"" submit file"
CHARACTER (LEN=*), PARAMETER :: ch_EM00003 = &
      "No ""$1"" file exists for this Blossom session"
CHARACTER (LEN=*), PARAMETER :: ch_EM00004 = &
      "Can't send note to output file if no data file has been USEd yet"
CHARACTER (LEN=*), PARAMETER :: ch_EM00005 = &
      "Problem accessing ""$1"" in $2"
CHARACTER (LEN=*), PARAMETER :: ch_EM00006 = &
      " No selection made"
CHARACTER (LEN=*), PARAMETER :: ch_EM00007 = &
      "No files matching ""$1"" to use"
CHARACTER (LEN=*), PARAMETER :: ch_EM00008 = &
      "Syntax is:$nSUBMIT <filename>$nor$nSUBMIT"
CHARACTER (LEN=*), PARAMETER :: ch_EM00009 = &
      "Submit file [<drive>:][<path>\]<filename> specification can't$n&
      &exceed $1 characters"
CHARACTER (LEN=*), PARAMETER :: ch_EM00010 = &
      "You cannot specify the ""$1"" history file as your submit file"
CHARACTER (LEN=*), PARAMETER :: ch_EM00011 = &
      "Submit file ""$1"" does not exist"
CHARACTER (LEN=*), PARAMETER :: ch_EM00012 = &
      "Problem opening ""$1"" as submit file"
CHARACTER (LEN=*), PARAMETER :: ch_EM00013 = &
      "The ""$1"" Blossom help file not found"
CHARACTER (LEN=*), PARAMETER :: ch_EM00014 = &
      "Problem opening ""$1"" Blossom help file"
CHARACTER (LEN=*), PARAMETER :: ch_EM00015 = &
      "$1$n^-- Help is not available for this"
CHARACTER (LEN=*), PARAMETER :: ch_EM00016 = &
      "Problem reading ""$1"" Blossom help file"
CHARACTER (LEN=*), PARAMETER :: ch_EM00017 = &
      "Problem reading ""$1"" Blossom help file$nError code: $2"
CHARACTER (LEN=*), PARAMETER :: ch_EM00018 = &
      "Problem deleting zero-length output file ""$1"""
CHARACTER (LEN=*), PARAMETER :: ch_EM00019 = &
      "Filename given to USE ""$1""$n &
      &exceeds Blossom maximum filename size of 25 characters"
!!x       &use DOS shortened filename in 8.3 format, such as$nMYLONG~1.DAT for &
!!x       &MYLONGFILENAME.DAT.$nUse DOS ""DIR"" command to get shortened filename"
CHARACTER (LEN=*), PARAMETER :: ch_EM00020 = &
      "Filename ""$1"" given to USE does not exist,$nor Blossom unable to f&
      &ind it"
CHARACTER (LEN=*), PARAMETER :: ch_EM00021 = &
      "Problem with specified USE file ""$1"""
CHARACTER (LEN=*), PARAMETER :: ch_EM00022 = &
      "Problem opening ""$1"" filename to USE"
CHARACTER (LEN=*), PARAMETER :: ch_EM00023 = &
      "Syntax is:$nUSE <filename>$n  or$nUSE <filename> / var1 [var2 ...]$n&
      &  or$nUSE"
CHARACTER (LEN=*), PARAMETER :: ch_EM00024 = &
      "Number of variable names must not exceed $1"
CHARACTER (LEN=*), PARAMETER :: ch_EM00025 = &
      "Variable names must begin with alphabetic character.$n""$1"" &
      &encountered"
CHARACTER (LEN=*), PARAMETER :: ch_EM00026 = &
      "Variable names may contain only alphanumeric characters or &
      &underscore.$n""$1"" encountered"
CHARACTER (LEN=*), PARAMETER :: ch_EM00027 = &
      "Unexpected End of File encountered reading USE file ""$1"""
CHARACTER (LEN=*), PARAMETER :: ch_EM00028 = &
      "Problem reading ""$1"" USE file"
CHARACTER (LEN=*), PARAMETER :: ch_EM00029 = &
      "Non-valid variable name ""$1"" encountered"
CHARACTER (LEN=*), PARAMETER :: ch_EM00030 = &
      "Problem opening ""$1"" output file"
CHARACTER (LEN=*), PARAMETER :: ch_EM00031 = &
      "Problem reading ""$1"" output file"
CHARACTER (LEN=*), PARAMETER :: ch_EM00032 = &
      "Problem opening ""$1"" Blossom temporary file"
CHARACTER (LEN=*), PARAMETER :: ch_EM00033 = &
      "Memory allocation error: $1 in $2"
CHARACTER (LEN=*), PARAMETER :: ch_EM00034 = &
      "More data elements ($1) than number of variables ($2)$nfor case $3 &
      &in USE file ""$4"""
CHARACTER (LEN=*), PARAMETER :: ch_EM00035 = &
      "Illegal data encountered: ""$1"" for case: $2"
CHARACTER (LEN=*), PARAMETER :: ch_EM00036 = &
      "Problem writing to Blossom file ""$1"" at Record Number: $2"
CHARACTER (LEN=*), PARAMETER :: ch_EM00037 = &
      "Syntax is:$nSAVE <filename>"
CHARACTER (LEN=*), PARAMETER :: ch_EM00038 = &
      "There is no USE data file specified. Cannot save a labeled data &
      &file"
CHARACTER (LEN=*), PARAMETER :: ch_EM00039 = &
      "The USE file ""$1"" is already a label file"
CHARACTER (LEN=*), PARAMETER :: ch_EM00040 = &
      "The SAVE file name must be different from your USE file: ""$1"""
CHARACTER (LEN=*), PARAMETER :: ch_EM00041 = &
      "Problem with specified SAVE file ""$1"""
CHARACTER (LEN=*), PARAMETER :: ch_EM00042 = &
      "Problem creating ""$1"" Save file in $2"
CHARACTER (LEN=*), PARAMETER :: ch_EM00043 = &
      "Problem opening ""$1"" USE file for SAVE.$nBlossom cannot &
      &save ""$2"""
CHARACTER (LEN=*), PARAMETER :: ch_EM00044 = &
      "Problem reading ""$1"" USE file for SAVE.$nBlossom cannot &
      &save ""$2"""
CHARACTER (LEN=*), PARAMETER :: ch_EM00045 = &
      "Syntax is:$nOUTPUT <filename>$n or$nOUTPUT / [TERSE|VERBOSE]$n or$n&
      &OUTPUT <filename> / [TERSE|VERBOSE]"
CHARACTER (LEN=*), PARAMETER :: ch_EM00046 = &
      "Problem opening ""$1"" Blossom output file.$nAttempting to continue &
      &processing"
CHARACTER (LEN=*), PARAMETER :: ch_EM00047 = &
      "Problem opening ""$1"" file in $2"
CHARACTER (LEN=*), PARAMETER :: ch_EM00048 = &
      "Problem reading ""$1"" file in $2:$nCase: $3$nBlock: $4$nVariable: &
      &$5$nRecord: $6"
CHARACTER (LEN=*), PARAMETER :: ch_EM00049 = &
      "Problem writing ""$1"" file in $2:$nCase: $3$nBlock: $4$nVariable: &
      &$5$nRecord: $6"
CHARACTER (LEN=*), PARAMETER :: ch_EM00050 = &
      "Problem reading ""$1"" file in $2"
CHARACTER (LEN=*), PARAMETER :: ch_EM00051 = &
      "Cases with missing values were dropped from this dataset"
CHARACTER (LEN=*), PARAMETER :: ch_EM00052 = &
      "Missing value found in data"
CHARACTER (LEN=*), PARAMETER :: ch_EM00053 = &
      "Observation number $1"
CHARACTER (LEN=*), PARAMETER :: ch_EM00054 = &
      "Grouping value: $1"
CHARACTER (LEN=*), PARAMETER :: ch_EM00055 = &
      "Blocking value: $1"
CHARACTER (LEN=*), PARAMETER :: ch_EM00056 = &
      "You cannot have missing values with the blocked procedure as this &
      &would cause$nan unbalanced design. Check the data set and remove &
      &missing values"
CHARACTER (LEN=*), PARAMETER :: ch_EM00057 = &
      "Variable name must begin with letter,$nnot ""$1"" in variable name&
      & ""$2"""
CHARACTER (LEN=*), PARAMETER :: ch_EM00058 = &
      "Illegal character ""$1"" was found$nin variable name ""$2"""
CHARACTER (LEN=*), PARAMETER :: ch_EM00059 = &
      "Variable name ""$1"" is not in list of variables from USE command"
CHARACTER (LEN=*), PARAMETER :: ch_EM00060 = &
      "Expecting $1 but encountered ""$2"""
CHARACTER (LEN=*), PARAMETER :: ch_EM00061 = &
      "Expecting a number, but encountered ""$1"""
CHARACTER (LEN=*), PARAMETER :: ch_EM00062 = &
      "Expecting ""("", ""*"", ""/"" (or nothing), but encountered ""$1"""
CHARACTER (LEN=*), PARAMETER :: ch_EM00063 = &
      "There appears to be more than one observation per block. MRBP &
      &cannot analyze$nsuch data. You could average the observations in a &
      &block, and enter data$nthat way if you want to use MRBP.&
      & It is also possible to align and collapse$nthe analysis to MRPP &
      &(Mielke and Iyer, 1982)"
CHARACTER (LEN=*), PARAMETER :: ch_EM00064 = &
      "There should be the same number of blocks for each group.$nGroup &
      &with value $1 has $2 blocks, while$ngroup with value $3 has $4 blocks"
CHARACTER (LEN=*), PARAMETER :: ch_EM00065 = &
      "Grouping variable not found in list of variables from USE command"
CHARACTER (LEN=*), PARAMETER :: ch_EM00066 = &
      "Problem with grouping and blocking variable names"
CHARACTER (LEN=*), PARAMETER :: ch_EM00067 = &
      "Problem reading ""$1"" file at Observation (Case): $2"
CHARACTER (LEN=*), PARAMETER :: ch_EM00068 = &
      "No variable names were encountered before ""$1"""
CHARACTER (LEN=*), PARAMETER :: ch_EM00069 = &
      "A ""$1"" was encountered unexpectedly"
CHARACTER (LEN=*), PARAMETER :: ch_EM00070 = &
      "No variable names were specified"
CHARACTER (LEN=*), PARAMETER :: ch_EM00071 = &
      "Maximum number of variable names allowed with MRPP is $1"
CHARACTER (LEN=*), PARAMETER :: ch_EM00072 = &
      "Problem creating All Quantile SAVE file $1 in $2"
CHARACTER (LEN=*), PARAMETER :: ch_EM00076 = &
      "Expecting ""="" but encountered ""$1"""
CHARACTER (LEN=*), PARAMETER :: ch_EM00078 = &
      "Expecting ""$1"" but encountered ""$2"""
CHARACTER (LEN=*), PARAMETER :: ch_EM00080 = &
      "Missing value found, Case: $1$nVariable: ""$2"""
CHARACTER (LEN=*), PARAMETER :: ch_EM00089 = &
      "Problem reading ""$1"" file in $2"
CHARACTER (LEN=*), PARAMETER :: ch_EM00101 = &
      "Memory allocation error: $1 in command line in $2$nProblem in &
      &COVER:$n$3"
CHARACTER (LEN=*), PARAMETER :: ch_EM00102 = &
      "Problem in COVER:$n$1"
CHARACTER (LEN=*), PARAMETER :: ch_EM00103 = &
      "Expecting ""*"" or ""/"" (or nothing), but encountered: ""$1""$n&
      &Problem in COVER:$n$2"
CHARACTER (LEN=*), PARAMETER :: ch_EM00104 = &
      "Unknown option encountered: ""$1"""
CHARACTER (LEN=*), PARAMETER :: ch_EM00105 = &
      "Problem with grouping variable name"
CHARACTER (LEN=*), PARAMETER :: ch_EM00106 = &
      "Problem with Cover variable name"
CHARACTER (LEN=*), PARAMETER :: ch_EM00107 = &
      "No Coverage variable name specified"
CHARACTER (LEN=*), PARAMETER :: ch_EM00108 = &
      "Coverage variable name ""$1"" not in USEd file's variable list"
CHARACTER (LEN=*), PARAMETER :: ch_EM00109 = &
      "There is only $1 group in the selected data set.$nThere must be at &
      &least $2 values of the grouping variable to do COVER test"
CHARACTER (LEN=*), PARAMETER :: ch_EM00110 = &
      "Group with value $1 has only $2 observation(s).$nYou must have at &
      &least $3 observations in each group"
CHARACTER (LEN=*), PARAMETER :: ch_EM00111 = &
      "Problem creating ""$1"" file in $2"
CHARACTER (LEN=*), PARAMETER :: ch_EM00112 = &
      "Number of cases (observations) should not exceed $1 for$nKendall-&
      &Sherman Goodness of Fit"
CHARACTER (LEN=*), PARAMETER :: ch_EM00113 = &
      "Number of cases (observations) should not exceed $1 for$nG-Sample &
      &Empirical Sequence Test"
CHARACTER (LEN=*), PARAMETER :: ch_EM00114 = &
      "Number of groups should not exceed $1 for$nG-Sample Empirical &
      &Sequence Test"
CHARACTER (LEN=*), PARAMETER :: ch_EM00115 = &
      "Number of Cover variables should not exceed $1 for$nG-Sample &
      &Empirical Sequence Test"
CHARACTER (LEN=*), PARAMETER :: ch_EM00116 = &
      "You must give a LAD command before HYPOTHESIS command"
CHARACTER (LEN=*), PARAMETER :: ch_EM00117 = &
      "Problem in LAD"
CHARACTER (LEN=*), PARAMETER :: ch_EM00118 = &
      "Problem in HYPOTHESIS command line (variables):$n$1"
CHARACTER (LEN=*), PARAMETER :: ch_EM00119 = &
      "You are trying to use $1 variables$nMaximum number of variables with&
      & LAD is $2"
CHARACTER (LEN=*), PARAMETER :: ch_EM00120 = &
      "No independent variable name specified"
CHARACTER (LEN=*), PARAMETER :: ch_EM00121 = &
      "HYPOTHESIS dependent variable name ""$1""$ndoes not match LAD &
      &dependent variable name ""$2"""
CHARACTER (LEN=*), PARAMETER :: ch_EM00122 = &
      "Expecting LAD save file name after ""="""
CHARACTER (LEN=*), PARAMETER :: ch_EM00123 = &
      "Can't do Quantile least squares regression"
CHARACTER (LEN=*), PARAMETER :: ch_EM00124 = &
      "Quantiles must be between 0.0 and 1.0"
CHARACTER (LEN=*), PARAMETER :: ch_EM00125 = &
      """$1"" Save file must be different from USE or SAVE file"
CHARACTER (LEN=*), PARAMETER :: ch_EM00126 = &
      "Problem with specified ""$1"" Save file ""$2"""
CHARACTER (LEN=*), PARAMETER :: ch_EM00127 = &
      "There is no USE file specified"
CHARACTER (LEN=*), PARAMETER :: ch_EM00128 = &
      "Problem writing HOLD temporary data file"
CHARACTER (LEN=*), PARAMETER :: ch_EM00129 = &
      "Missing value found in HYPOTHESIS data at Observation: $1$n&
      &Hypothesis would then have a different number of cases than LAD, so &
      &a$nreduced model test would not be meaningful.$nCheck your data &
      &and remove missing values"
CHARACTER (LEN=*), PARAMETER :: ch_EM00130 = &
      "No cases with non-missing values left"
CHARACTER (LEN=*), PARAMETER :: ch_EM00131 = &
      "For hypothesis testing, in reduced model there were no $nvariables &
      &dropped from full model!"
CHARACTER (LEN=*), PARAMETER :: ch_EM00132 = &
      "Problem reading $1: Record # $2"
CHARACTER (LEN=*), PARAMETER :: ch_EM00133 = &
      "Problem with LAD in RUNLAD"
CHARACTER (LEN=*), PARAMETER :: ch_EM00134 = &
      "The CONSTANT term was not in the LAD model.$nVariables in reduced &
      &model must be a subset of variables in full model"
CHARACTER (LEN=*), PARAMETER :: ch_EM00135 = &
      "Variable ""$1"" is not in full model.$nVariables in reduced model &
      &must be a subset of variables in full model"
CHARACTER (LEN=*), PARAMETER :: ch_EM00136 = &
      "You have $1 cases$nNumber of cases (observations) should not exceed &
      &$2 for LAD"
CHARACTER (LEN=*), PARAMETER :: ch_EM00137 = &
      "You have $1 response variables$nNumber of response variables should &
      &not exceed $2 for LAD"
CHARACTER (LEN=*), PARAMETER :: ch_EM00138 = &
      "Problem in MEDQ:$n$1$nMEDQ syntax:$nMEDQ <variable list> [ * &
      &GROUPING_VARIABLE [<gvspec>] ] [ / <options> ]"
CHARACTER (LEN=*), PARAMETER :: ch_EM00139 = &
      "Expecting MEDQ Distances SAVE filename but encountered ""$1"""
CHARACTER (LEN=*), PARAMETER :: ch_EM00140 = &
      "Expecting list of quantile values but encountered ""$1"""
CHARACTER (LEN=*), PARAMETER :: ch_EM00141 = &
      "No Quantile values seen in command"
CHARACTER (LEN=*), PARAMETER :: ch_EM00142 = &
      "Problem allocating memory for specified quantile values"
CHARACTER (LEN=*), PARAMETER :: ch_EM00143 = &
      "The filename ""$1"" you gave to save the median distances is the &
      &name$nof a Blossom system file. You can't use this as a filename here"
CHARACTER (LEN=*), PARAMETER :: ch_EM00144 = &
      "Problem with the filename ""$1"" you gave to save median distances"
CHARACTER (LEN=*), PARAMETER :: ch_EM00145 = &
      "Your distance exponent ""$1"" is zero or close to zero.$nBlossom &
      &won't allow a distance exponent of zero or that close to zero"
CHARACTER (LEN=*), PARAMETER :: ch_EM00146 = &
      "Improper syntax for paired MRPP command.$nSyntax is:$nMRPP var1 var2&
      & / PAIRED [v=dist]"
CHARACTER (LEN=*), PARAMETER :: ch_EM00147 = &
      "Expecting ""*"" followed by grouping variable name"
CHARACTER (LEN=*), PARAMETER :: ch_EM00148 = &
      "Expecting ""*"" or ""/"" (or nothing), but encountered ""$1"""
CHARACTER (LEN=*), PARAMETER :: ch_EM00149 = &
      "Can't use Hotelling's commensuration with a randomized block &
      &experiment (MRBP)"
CHARACTER (LEN=*), PARAMETER :: ch_EM00150 = &
      "Problem in MRPP:$n$1"
CHARACTER (LEN=*), PARAMETER :: ch_EM00151 = &
      "ARC distances are supported for univariate MRPP only"
CHARACTER (LEN=*), PARAMETER :: ch_EM00152 = &
      "Valid values for C are $1, $2, $3, or $4"
CHARACTER (LEN=*), PARAMETER :: ch_EM00153 = &
      "Problem screening for missing values in MRDOSET"
CHARACTER (LEN=*), PARAMETER :: ch_EM00154 = &
      "There is (are) only $1 group(s) in the specified grouping variable &
      &data$nThere must be at least $2$nPerhaps (some) specified group(s) &
      &contain missing values. Check your data"
CHARACTER (LEN=*), PARAMETER :: ch_EM00155 = &
      "There are only $1 cases in this data set.$nThere must be $2 or more &
      &cases to do an MRPP analysis.$nCheck your data for missing values"
CHARACTER (LEN=*), PARAMETER :: ch_EM00156 = &
      "Group with value $1 contains only one observation.$nGroups must have&
      & $2 or more observations to do an MRPP.$nCheck your data for missing&
      & values"
CHARACTER (LEN=*), PARAMETER :: ch_EM00157 = &
      "For Hotelling commensuration the number of cases must be greater &
      &than$nthe number of variables"
CHARACTER (LEN=*), PARAMETER :: ch_EM00158 = &
      "You can only do Hotelling commensuration with multivariate data"
CHARACTER (LEN=*), PARAMETER :: ch_EM00159 = &
      "There are $1 cases in the Excess group.$nThere must be at least $2 &
      &cases with non-missing values to do an MRPP.$nCheck your data"
CHARACTER (LEN=*), PARAMETER :: ch_EM00160 = &
      "There is only $1 group in the specified grouping variable data.$n&
      &There must be at least $2 grouping variable values to do an Exact MRPP"
CHARACTER (LEN=*), PARAMETER :: ch_EM00161 = &
      "There are only $1 cases in this data set.$nThere must be $2 or more &
      &cases to do an exact MRPP"
CHARACTER (LEN=*), PARAMETER :: ch_EM00162 = &
      "Group with value $1 contains only $2 observation(s).$nGroups must &
      &have $3 or more observations to do an exact MRPP"
CHARACTER (LEN=*), PARAMETER :: ch_EM00163 = &
      "There are $1 cases in the Excess group.$nThere must be at least $2 &
      &cases with non-missing values to do an Exact MRPP.$nCheck your data"
CHARACTER (LEN=*), PARAMETER :: ch_EM00164 = &
      "There is only $1 group(s) in the specified grouping variable data.$n&
      &There must be at least $2 grouping variable values to do an MRBP"
CHARACTER (LEN=*), PARAMETER :: ch_EM00165 = &
      "There is only $1 block(s) in the specified blocking variable data.$n&
      &There must be at least $2 blocking variable values to do an MRBP"
CHARACTER (LEN=*), PARAMETER :: ch_EM00166 = &
      "There are only $1 pairs of observations in the data.$nThere should &
      &be at least $2 pairs in order to do a PTMP"
CHARACTER (LEN=*), PARAMETER :: ch_EM00167 = &
      "You have $1 cases.$nNumber of cases (observations) should not exceed&
      & $2 for$nMRPP of this type"
CHARACTER (LEN=*), PARAMETER :: ch_EM00168 = &
      "You have $1 groups.$nNumber of groups should not exceed $2 for MRPP &
      &of this type"
CHARACTER (LEN=*), PARAMETER :: ch_EM00169 = &
      "You have $1 variables$nNumber of variables should not exceed $2 for &
      &MRPP of this type"
CHARACTER (LEN=*), PARAMETER :: ch_EM00170 = &
      "You have $1 cases$nNumber of cases (observations) should not exceed &
      &$2 for Exact MRPP"
CHARACTER (LEN=*), PARAMETER :: ch_EM00171 = &
      "You have $1 groups.$nNumber of groups should not exceed $2 for Exact&
      & MRPP"
CHARACTER (LEN=*), PARAMETER :: ch_EM00172 = &
      "You have $1 variables.$nNumber of variables should not exceed $2 for&
      & Exact MRPP"
CHARACTER (LEN=*), PARAMETER :: ch_EM00173 = &
      "There is a limit of $1 blocks for an Exact Multi-Response$nBlocked &
      &Procedure"
CHARACTER (LEN=*), PARAMETER :: ch_EM00174 = &
      "The dimensions of your data exceed those available from the$nBlocked&
      & Procedure for Multi-Responses (MRBP) analytical$nprograms.  The &
      &maximum dimensions may not exceed:$n$1 Observations,$n$2 Groups,&
      &$n$3 Response Variables, or$n$4 Blocks.$nYour data for analysis &
      &contains:$n$5 Observations,$n$6 Groups,$n$7 Response Variables, &
      &and$n$8 Blocks"
CHARACTER (LEN=*), PARAMETER :: ch_EM00175 = &
      "You have $1 cases$nNumber of cases (observations) should not exceed &
      &$2 for Paired MRPP"
CHARACTER (LEN=*), PARAMETER :: ch_EM00176 = &
      "Problem from MRPP program: MRPP had a problem reading data"
CHARACTER (LEN=*), PARAMETER :: ch_EM00177 = &
      "Problem from MRPP program: MRPP could not allocate memory for &
      &analysis"
CHARACTER (LEN=*), PARAMETER :: ch_EM00178 = &
      "Problem from MRPP program: The Variance/Covariance Matrix was &
      &singular.$nHotelling's commensuration can't be done"
CHARACTER (LEN=*), PARAMETER :: ch_EM00179 = &
      "Analysis could not proceed"
CHARACTER (LEN=*), PARAMETER :: ch_EM00180 = &
      "Expecting either ""/"" (or nothing) but encountered ""$1"""
CHARACTER (LEN=*), PARAMETER :: ch_EM00181 = &
      "Problem in SPP command:$n$1"
CHARACTER (LEN=*), PARAMETER :: ch_EM00182 = &
      "Problem opening ""$1"" submit file in DOMRSP"
CHARACTER (LEN=*), PARAMETER :: ch_EM00183 = &
      " "
CHARACTER (LEN=*), PARAMETER :: ch_EM00184 = &
      "Problem in SPDOSET: getting Sequence variable values"
CHARACTER (LEN=*), PARAMETER :: ch_EM00185 = &
      "You have $1 cases (observations) in this analysis.$nThere should be &
      &at least $2 cases to do an$nExact Multi-Response Sequence Procedure"
CHARACTER (LEN=*), PARAMETER :: ch_EM00186 = &
      "You have $1 observations.$nNumber of cases (observations) should not&
      & exceed $2 for Multi-Response$nSequence Procedures"
CHARACTER (LEN=*), PARAMETER :: ch_EM00187 = &
      "Number of cases (observations) must be at least $1$nfor Multi-&
      &Response Sequence Procedures"
CHARACTER (LEN=*), PARAMETER :: ch_EM00188 = &
      "You have $1 response variables.$nNumber of response variables should&
      & not exceed $2 for Multi-Response$nSequence Procedures"
CHARACTER (LEN=*), PARAMETER :: ch_EM00190 = &
      "Expecting either ""="" (or nothing) but encountered ""$1"""
CHARACTER (LEN=*), PARAMETER :: ch_EM00191 = &
      "The filename ""$1"" you gave to save the median distances is the &
      &name$nof your USEd filename. You can't use this as a filename here"
CHARACTER (LEN=*), PARAMETER :: ch_EM00192 = &
      "$1: Record # $2"
CHARACTER (LEN=*), PARAMETER :: ch_EM00193 = &
      "Syntax is:$nSP var1 [var2 ...] [*seq_var] [/] [V=<dist val>] [NOCOM]&
      & [EXACT]"
CHARACTER (LEN=*), PARAMETER :: ch_EM00194 = &
      "$1 getting Sequence variable values"
CHARACTER (LEN=*), PARAMETER :: ch_EM00195 = &
      "Number of cases (observations) must be at least $1$nfor Exact Multi-&
      &Response Sequence Procedures"
CHARACTER (LEN=*), PARAMETER :: ch_EM00196 = &
      "The filename you gave, ""$1"", is a Blossom system filename.$nYou &
      &can't use the name of a Blossom system file for a SUBMIT filename."
CHARACTER (LEN=*), PARAMETER :: ch_EM00197 = &
      "Can''t /SAVE from Hypothesis test regression$n&
      &No SAVE file will be made."
CHARACTER (LEN=*), PARAMETER :: ch_EM00198 = &
      "Bad command from Blossom program command arguments:$n""$1"""
CHARACTER (LEN=*), PARAMETER :: ch_EM00199 = &
      "No filename given to use"
CHARACTER (LEN=*), PARAMETER :: ch_EM00200 = &
      "You can't have missing values with a Sequence Procedure"
CHARACTER (LEN=*), PARAMETER :: ch_EM00201 = &
      "Problem reading ""$1"" file in $2"
CHARACTER (LEN=*), PARAMETER :: ch_EM00202 = &
      "You must have more than 1 and fewer than 2147483647 quantile values"
CHARACTER (LEN=*), PARAMETER :: ch_EM00203 = &
      "Quantile values must be between 0.0 and 1.0"
CHARACTER (LEN=*), PARAMETER :: ch_EM00204 = &
      "You must have at least 1 and fewer than $1 variables"
CHARACTER (LEN=*), PARAMETER :: ch_EM00205 = &
      "You must have more than 2 and fewer than 2147483647 observations"
CHARACTER (LEN=*), PARAMETER :: ch_EM00206 = &
      "You must have more than 1 and fewer than 2147483647 groups for &
      &grouped data"
CHARACTER (LEN=*), PARAMETER :: ch_EM00207 = &
      "You must have more than 2 and fewer than 2147483647 observations &
      &in a group"
CHARACTER (LEN=*), PARAMETER :: ch_EM00208 = &
      "The maximum number of independent variables in a LAD regression &
      &is set at $1"
CHARACTER (LEN=*), PARAMETER :: ch_EM00209 = &
      "You have $1 cases and $2 LAD regression variables$nThe number of cases &
      &(observations) should be larger than$nthe number of LAD regression &
      &variables"
CHARACTER (LEN=*), PARAMETER :: ch_EM00210 = &
      "You have $1 LAD regression variables$nThe number of LAD regression &
      &variables can't exceed $2 in Blossom"

CHARACTER (LEN=*), PARAMETER :: ch_EM00211 = &
      "Blossom may not be installed properly.$n&
      &Processing continues although the program may not run correctly$n&
      &or may terminate unexpectedly.$n&
      &However:$n&
      &If you had an older version of Blossom installed,$n&
      &Right click on 'My Computer', then select Properties,$n&
      &Advanced Tab, Environment Variables, and delete$n&
      &the User variable 'BLOSSDIR'. (Do *NOT* delete the$n&
      &System variable 'BLOSSDIR'.) Then Restart your computer."

CHARACTER (LEN=*), PARAMETER :: ch_EM00213 = &
      "The variables used with the Hypothesis command be a subset of the$n&
      &variables used in the original regression command. You specified$n&
      &$s1 variables in the original command and $s2 variables in the$n&
      &Hypothesis command.  You must have fewer variables specified in$n&
      &the Hypothesis command"
CHARACTER (LEN=*), PARAMETER :: ch_EM000214 = &
      "Problem obtaining variable names from command line"
CHARACTER (LEN=*), PARAMETER :: ch_EM000215 = &
      "Problem creating temporary file ""$1"" in TSELECT"
CHARACTER (LEN=*), PARAMETER :: ch_EM000216 = &
      "Error from shell to DOS prompt"
CHARACTER (LEN=*), PARAMETER :: ch_EM000217 = &
      "Problem opening ""$1"" Blossom LOG file$nAttempting to continue &
      &processing"
CHARACTER (LEN=*), PARAMETER :: ch_EM000218 = &
      "Problem opening ""$1"" statistics output file.$n&
      &Attempting to continue processing"
CHARACTER (LEN=*), PARAMETER :: ch_EM000219 = &
      "COMSPEC can't be greater than $1 characters"
CHARACTER (LEN=*), PARAMETER :: ch_EM000220 = &
      "Submit file ""$1"" should exist but does not"
CHARACTER (LEN=*), PARAMETER :: ch_EM000221 = &
      "Syntax is:$nECHO [DATA=ON|OFF] [OUTPUT=ON|OFF]$n  or$nECHO DEFAULT"
CHARACTER (LEN=*), PARAMETER :: ch_EM000222 = &
      "Problem opening SIMOUT in BLOSSOM main"
CHARACTER (LEN=*), PARAMETER :: ch_EM00223 = &
      "Syntax is:$nRANDOM = DEFAULT$nor$nRANDOM = MT"
CHARACTER (LEN=*), PARAMETER :: ch_EM00224 = &
      "Expecting SAVETEST filename after '='"
CHARACTER (LEN=*), PARAMETER :: ch_EM00225 = &
      "Blossom can't do any hypothesis tests for an ""all quantile"" regression."
CHARACTER (LEN=*), PARAMETER :: ch_EM00226 = &
      "You can't get all quantile regressions with Hypothesis Command.$n&
      & You can't do hypothesis test of an ""all quantile"" regression."
CHARACTER (LEN=*), PARAMETER :: ch_EM00227 = &
      "Save file specification must be file name only, no path info.$n&
      & You specified ""$1"""
CHARACTER (LEN=*), PARAMETER :: ch_EM00228 = &
      "SAVETEST file specification must be file name only, no path info.$n&
      & You specified ""$1"""
        !
        !   -= PROMPT and DIALOG STRINGS (for emitstr) =-
CHARACTER (LEN=*), PARAMETER :: ch_STR0000 = &
      ">"
CHARACTER (LEN=*), PARAMETER :: ch_STR0001 = &
      "$1"
CHARACTER (LEN=*), PARAMETER :: ch_STR0002 = &
!       "$nReturning to keyboard input"
      "$nReturning to user input"
CHARACTER (LEN=*), PARAMETER :: ch_STR0003 = &
      "The specified SAVE file ""$1"" exists already.$nDo you wish to &
      &overwrt it? (Y/N): "
CHARACTER (LEN=*), PARAMETER :: ch_STR0004 = &
      "Y"
CHARACTER (LEN=*), PARAMETER :: ch_STR0005 = &
      "Cases with missing values were dropped from this dataset"
CHARACTER (LEN=*), PARAMETER :: ch_STR0006 = &
      "Output was appended to file $1"
CHARACTER (LEN=*), PARAMETER :: ch_STR0007 = &
      "Unique values found"
CHARACTER (LEN=*), PARAMETER :: ch_STR0008 = &
      "Fisher''s exact not implemented in this version of Blossom"
CHARACTER (LEN=*), PARAMETER :: ch_STR0009 = &
      "For the Exact G-Sample Empirical Sequence Test$nthe number of &
      &permutations goes&
      & up really fast.$nThis may take awhile..."
CHARACTER (LEN=*), PARAMETER :: ch_STR0010 = &
      "Computation of Exact G-Sample Empirical Sequence Test may take a &
      &long time.$n&
      &Do you want to continue for your N = $1 observations? (Y/N):"
CHARACTER (LEN=*), PARAMETER :: ch_STR0011 = &
      "N"
CHARACTER (LEN=*), PARAMETER :: ch_STR0012 = &
      "The specified ""$1"" Save file ""$2"" exists already.$nDo you wish to &
      &overwrt it? (Y/N): "
CHARACTER (LEN=*), PARAMETER :: ch_STR0013 = &
      "This may take awhile..."
CHARACTER (LEN=*), PARAMETER :: ch_STR0014 = &
      "Shelling to the Operating System"
CHARACTER (LEN=*), PARAMETER :: ch_STR0016 = &
      "Enter 'EXIT' when you want to return to Blossom"
CHARACTER (LEN=*), PARAMETER :: ch_STR0017 = &
      "Current COMSPEC (path to CMD.EXE) is:$n$1$nEnter new path to &
      &CMD.EXE or enter 'EXIT' to return to Blossom: "
CHARACTER (LEN=*), PARAMETER :: ch_STR0018 = &
      "Welcome back to Blossom"
CHARACTER (LEN=*), PARAMETER :: ch_STR0019 = &
      "Press <ENTER> for next page..."
CHARACTER (LEN=*), PARAMETER :: ch_STR0020 = &
      "Enter filename to use: "
CHARACTER (LEN=*), PARAMETER :: ch_STR0021 = &
      "$1 $2"
CHARACTER (LEN=*), PARAMETER :: ch_STR0022 = &
      "There may be another instance of Blossom runnning.$n&
      &Only one Blossom session may run at any time.$n&
      &If you have another Blossom session running, QUIT this session and$n&
      &return to the first Blossom session for processing"
CHARACTER (LEN=*), PARAMETER :: ch_STR0023 = &
      "Press the <ENTER> key to quit Blossom..."
CHARACTER (LEN=*), PARAMETER :: ch_STR0024 = &
      "ECHO DATA input is now ON"
CHARACTER (LEN=*), PARAMETER :: ch_STR0025 = &
      "ECHO DATA input is now OFF"
CHARACTER (LEN=*), PARAMETER :: ch_STR0026 = &
      "Blossom does not use character variables, so $1 Systat$n&
      &character variable was skipped"
CHARACTER (LEN=*), PARAMETER :: ch_STR0027 = &
      "Blossom does not use character variables, so $1 Systat$n&
      &character variables were skipped"
CHARACTER (LEN=*), PARAMETER :: ch_STR0028 = &
      "Attempting to continue processing"
CHARACTER (LEN=*), PARAMETER :: ch_STR0029 = &
      "NOALIGN option for Blocked procedures only.$nNOALIGN ignored"
CHARACTER (LEN=*), PARAMETER :: ch_STR0030 = &
      "Distances to multivariate median were written to labeled file:"
CHARACTER (LEN=*), PARAMETER :: ch_STR0031 = &
      "Computation times for Exact Sequence Procedures is proportional to N!&
      &$n(N Factorial). For N = Number of Observations and one Response &
      &Variable,$nthe following run times are estimated for a 500MHz Pentium &
      &III:$nN   RUN TIME$n6   0.05 seconds$n7   0.1  seconds$n8   1.0  &
      &seconds$n9   10.5 seconds$n10  2  minutes$n11  26 minutes$n12  5 hours &
      &8 minutes$n13  2 days 19 hours$n14  1 month$n15  1 year 3 months$nDo &
      &you want to continue for your N = $1 observations? (Y/N):$n"
CHARACTER (LEN=*), PARAMETER :: ch_STR0032 = &
      "There may be another instance of Blossom runnning in the current &
      &directory.$nOnly one Blossom session may run at any time in a single &
      &directory.$nIf you have another Blossom session running, QUIT this &
      &session and$nreturn to the first Blossom session for processing."
CHARACTER (LEN=*), PARAMETER :: ch_STR0033 = &
      "Problem renaming TEMP file ""$1"""
CHARACTER (LEN=*), PARAMETER :: ch_STR0034 = &
      "Problem renaming HOLD file ""$1"""
CHARACTER (LEN=*), PARAMETER :: ch_STR0035 = &
      "Problem renaming DUMMY file ""$1"""
CHARACTER (LEN=*), PARAMETER :: ch_STR0036 = &
      "ECHO OUTPUT results is now ON"
CHARACTER (LEN=*), PARAMETER :: ch_STR0037 = &
      "ECHO OUTPUT results is now OFF"
        !
        !   -= ERROR MESSAGES from Statistics Procedures (for errhand): =-
        ! Errors form cmsramod (Commensuration module):
INTEGER, PARAMETER :: i_cmsramod_ER001 = 10001
CHARACTER (LEN=*), PARAMETER :: ch_cmsramod_EM001 = &
      "Average distance for a variable is zero or near zero. To divide by &
      &this for$ncommensuration would result in very large data values for &
      &the analytical$nprogram. This suggests the values are very close in &
      &the data space. Inspect$nyour data for errors, or for a variable &
      &with very small variability.  You$nmight want to discard this &
      &variable in your analysis"
        !
        ! Errors from covermod (Cover statistics module):
INTEGER, PARAMETER :: i_covermod_ER001 = 20001
CHARACTER (LEN=*), PARAMETER :: ch_covermod_EM001 = &
      "You have $1 groups which is less than $2 required for a$nunivariate &
      &G-Sample Empirical Coverage Test"
INTEGER, PARAMETER :: i_covermod_ER002 = 20002
CHARACTER (LEN=*), PARAMETER :: ch_covermod_EM002 = &
      "Memory allocation errror: da_R,da_X,da_C in GSECT1"
INTEGER, PARAMETER :: i_covermod_ER003 = 20003
CHARACTER (LEN=*), PARAMETER :: ch_covermod_EM003 = &
      "Memory allocation errror: ia_A,ia_P in EGSECT1/egsect1perm"
INTEGER, PARAMETER :: i_covermod_ER004 = 20004
CHARACTER (LEN=*), PARAMETER :: ch_covermod_EM004 = &
      "Memory allocation error: da_C,da_X in EGSECT1/egsect1dval"
INTEGER, PARAMETER :: i_covermod_ER005 = 20005
CHARACTER (LEN=*), PARAMETER :: ch_covermod_EM005 = &
      "Memory allocation error: da_C in KSGF"
INTEGER, PARAMETER :: i_covermod_ER006 = 20006
CHARACTER (LEN=*), PARAMETER :: ch_covermod_EM006 = &
      "Memory allocation errror: da_R,da_X,da_C in EGSECT1"
        !
        ! Errors from medqmod (Multivariate Median/Quantile module):
INTEGER, PARAMETER :: i_MEDQMOD_ER001 = 30001
CHARACTER (LEN=*), PARAMETER :: ch_MEDQMOD_EM001 = &
      "Problem in MEDQUANT, You specified a quantile outside the range 0.0 &
      &to 1.0"
INTEGER, PARAMETER :: i_MEDQMOD_ER002 = 30002
CHARACTER (LEN=*), PARAMETER :: ch_MEDQMOD_EM002 = &
      "Memory allocation error: da_V,ia_OriOr in RMEDQ1"
INTEGER, PARAMETER :: i_MEDQMOD_ER003 = 30003
CHARACTER (LEN=*), PARAMETER :: ch_MEDQMOD_EM003 = &
      "Memory allocation error: da_W1,ia_OriOr in RMEDQ"
  ! Errors from mrppmod (Permutation Procedures module):
INTEGER, PARAMETER :: i_MRPPMOD_ER001 = 40001
CHARACTER (LEN=*), PARAMETER :: ch_MRPPMOD_EM001 = &
      "Memory allocation error: da_YHotHold in MRPP"
INTEGER, PARAMETER :: i_MRPPMOD_ER002 = 40002
CHARACTER (LEN=*), PARAMETER :: ch_MRPPMOD_EM002 = &
      "Memory allocation error: da_C,da_DSub1,da_DSub2 in MRPP"
INTEGER, PARAMETER :: i_MRPPMOD_ER003 = 40003
CHARACTER (LEN=*), PARAMETER :: ch_MRPPMOD_EM003 = &
      "Memory allocation error: da_UHot in MRPPDIST"
INTEGER, PARAMETER :: i_MRPPMOD_ER004 = 40004
CHARACTER (LEN=*), PARAMETER :: ch_MRPPMOD_EM004 = &
      "Memory allocation error: da_S,da_C,da_D in EMRPP"
INTEGER, PARAMETER :: i_MRPPMOD_ER005 = 40005
CHARACTER (LEN=*), PARAMETER :: ch_MRPPMOD_EM005 = &
      "Memory allocation error: da_YHotHold,da_UHot in EMRPP"
INTEGER, PARAMETER :: i_MRPPMOD_ER006 = 40006
CHARACTER (LEN=*), PARAMETER :: ch_MRPPMOD_EM006 = &
      "Memory allocation error: ia_A,ia_P,da_XI in EMRPPPERM"
INTEGER, PARAMETER :: i_MRPPMOD_ER007 = 40007
CHARACTER (LEN=*), PARAMETER :: ch_MRPPMOD_EM007 = &
      "Memory allocation error: da_XI in EMRPPDVAL"
INTEGER, PARAMETER :: i_MRPPMOD_ER008 = 40008
CHARACTER (LEN=*), PARAMETER :: ch_MRPPMOD_EM008 = &
      "Memory allocation error: da_X,da_Y,da_R,da_A,da_B in PTMP"
INTEGER, PARAMETER :: i_MRPPMOD_ER009 = 40009
CHARACTER (LEN=*), PARAMETER :: ch_MRPPMOD_EM009 = &
      "There are $1 non-zero differences in your data.$nThere must be at &
      &least $2 non-zero differences to do a Permutation Test$nfor Matched &
      &Pairs"
INTEGER, PARAMETER :: i_MRPPMOD_ER010 = 40010
CHARACTER (LEN=*), PARAMETER :: ch_MRPPMOD_EM010 = &
      "Memory allocation error: da_Dis in MRBPCALC"
INTEGER, PARAMETER :: i_MRPPMOD_ER011 = 40011
CHARACTER (LEN=*), PARAMETER :: ch_MRPPMOD_EM011 = &
      "Memory allocation error: da_SJ,da_SJ2,da_SJ3,da_SIJ,da_SIJ2,da_SIJ3,&
      &da_UIJ,da_TIJ2,da_TIJ3,da_VI in MRBPCALC"
INTEGER, PARAMETER :: i_MRPPMOD_ER012 = 40012
CHARACTER (LEN=*), PARAMETER :: ch_MRPPMOD_EM012 = &
      "Memory allocation error: da_AD,da_XM,da_X,da_S,ia_P2,ia_P3,ia_P4,&
      &ia_P5,ia_P6,ia_P7,ia_P8,ia_P9 IN EMRBP"
INTEGER, PARAMETER :: i_MRPPMOD_ER013 = 40013
CHARACTER (LEN=*), PARAMETER :: ch_MRPPMOD_EM013  = &
      "Exact MRBP must have a minimum of $1 blocks and$na maximum of $2 &
      &blocks.$nYou have $3 blocks"
INTEGER, PARAMETER :: i_MRPPMOD_ER014 = 40014
CHARACTER (LEN=*), PARAMETER :: ch_MRPPMOD_EM014 = &
      "Memory allocation error: da_RKS in EMRBPRANK"
INTEGER, PARAMETER :: i_MRPPMOD_ER015 = 40015
CHARACTER (LEN=*), PARAMETER :: ch_MRPPMOD_EM015 = &
      "Memory allocation error: ia_A in FACT2"
INTEGER, PARAMETER :: i_MRPPMOD_ER016 = 40016
CHARACTER (LEN=*), PARAMETER :: ch_MRPPMOD_EM016 = &
      "Memory allocation error: ia_A in FACT3"
INTEGER, PARAMETER :: i_MRPPMOD_ER017 = 40017
CHARACTER (LEN=*), PARAMETER :: ch_MRPPMOD_EM017 = &
      "Memory allocation error: ia_A in FACT4"
INTEGER, PARAMETER :: i_MRPPMOD_ER018 = 40018
CHARACTER (LEN=*), PARAMETER :: ch_MRPPMOD_EM018 = &
      "Memory allocation error: ia_A in FACT5"
INTEGER, PARAMETER :: i_MRPPMOD_ER019 = 40019
CHARACTER (LEN=*), PARAMETER :: ch_MRPPMOD_EM019 = &
      "Memory allocation error: ia_A in FACT6"
INTEGER, PARAMETER :: i_MRPPMOD_ER020 = 40020
CHARACTER (LEN=*), PARAMETER :: ch_MRPPMOD_EM020 = &
      "Memory allocation error: ia_A in FACT7"
INTEGER, PARAMETER :: i_MRPPMOD_ER021 = 40021
CHARACTER (LEN=*), PARAMETER :: ch_MRPPMOD_EM021 = &
      "Memory allocation error: ia_A in FACT8"
INTEGER, PARAMETER :: i_MRPPMOD_ER022 = 40022
CHARACTER (LEN=*), PARAMETER :: ch_MRPPMOD_EM022 = &
      "Memory allocation error: ia_A in FACT9"
INTEGER, PARAMETER :: i_MRPPMOD_ER023 = 40023
CHARACTER (LEN=*), PARAMETER :: ch_MRPPMOD_EM023 = &
      "Memory allocation error: da_X,da_S in EMRBPDVAL"
INTEGER, PARAMETER :: i_MRPPMOD_ER024 = 40024
CHARACTER (LEN=*), PARAMETER :: ch_MRPPMOD_EM024 = &
      "Memory allocation error: da_X in SPTMP"
INTEGER, PARAMETER :: i_MRPPMOD_ER025 = 40025
CHARACTER (LEN=*), PARAMETER :: ch_MRPPMOD_EM025 = &
      "Memory allocation error: da_DT in MRBP"
INTEGER, PARAMETER :: i_MRPPMOD_ER026 = 40026
CHARACTER (LEN=*), PARAMETER :: ch_MRPPMOD_EM026 = &
      "Memory allocation error: da_XI in MRPPDIST"
INTEGER, PARAMETER :: i_MRPPMOD_ER027 = 40027
CHARACTER (LEN=*), PARAMETER :: ch_MRPPMOD_EM027 = &
      "Memory allocation error: da_DataHold in MRPPDIST"
INTEGER, PARAMETER :: i_MRPPMOD_ER028 = 40028
CHARACTER (LEN=*), PARAMETER :: ch_MRPPMOD_EM028 = &
      "Memory allocation error: da_D in MRPP"
INTEGER, PARAMETER :: i_MRPPMOD_ER029 = 40029
CHARACTER (LEN=*), PARAMETER :: ch_MRPPMOD_EM029 = &
      "Memory allocation error: da_DPacked in MRPP"

        !
        ! Errors from invrtmod (Invert matrix module):
INTEGER, PARAMETER :: i_INVRTMOD_ER001 = 50001
CHARACTER (LEN=*), PARAMETER :: ch_INVRTMOD_EM001 = &
      "Memory allocation error: ia_KP,ia_KR,ia_KC in INVERT"
INTEGER, PARAMETER :: i_INVRTMOD_ER002 = 50002
CHARACTER (LEN=*), PARAMETER :: ch_INVRTMOD_EM002 = &
      "Memory allocation error: da_SSLA,da_SSLVW,ia_SSLIP in INVERT"
CHARACTER (LEN=*), PARAMETER :: ch_MAT_SINGULARITY = &
      "Either all the elements of some row were zero or a pivot became &
      &relatively$nzero. The variance/covariance matrix of variable &
      &values is probably singular$nand cannot be inverted. Hotelling &
      &commensuration cannot be done."
        !
        ! Errors from mrspmod (multi-response Sequence Procedures module):
INTEGER, PARAMETER :: i_MRSPMOD_ER002 = 60002
CHARACTER (LEN=*), PARAMETER :: ch_MRSPMOD_EM002 = &
      "Memory allocation error: da_Del in MRSPDIST"
INTEGER, PARAMETER :: i_MRSPMOD_ER003 = 60003
CHARACTER (LEN=*), PARAMETER :: ch_MRSPMOD_EM003 = &
      "Memory allocation error: da_DC in MRSPDIST"
INTEGER, PARAMETER :: i_MRSPMOD_ER004 = 60004
CHARACTER (LEN=*), PARAMETER :: ch_MRSPMOD_EM004 = &
      "Memory allocation error: da_AD,da_S in EMRSP"
INTEGER, PARAMETER :: i_MRSPMOD_ER006 = 60006
CHARACTER (LEN=*), PARAMETER :: ch_MRSPMOD_EM006 = &
      "Memory allocation error: ia_P,ia_A in EMRSPFACT"
INTEGER, PARAMETER :: i_MRSPMOD_ER007 = 60007
CHARACTER (LEN=*), PARAMETER :: ch_MRSPMOD_EM007 = &
      "Memory allocation error: da_S,da_X in EMRSPDVAL"
        !
        ! Errors from regrsmod (regression module):
INTEGER, PARAMETER :: i_REGSRMOD_ER001 = 70001
CHARACTER (LEN=*), PARAMETER :: ch_REGSRMOD_EM001 = &
      "Memory allocation error: da_DepVarVals in LAD"
INTEGER, PARAMETER :: i_REGSRMOD_ER002 = 70002
CHARACTER (LEN=*), PARAMETER :: ch_REGSRMOD_EM002 = &
      "Memory allocation error: da_AData_RS in LAD"
INTEGER, PARAMETER :: i_REGSRMOD_ER003 = 70003
CHARACTER (LEN=*), PARAMETER :: ch_REGSRMOD_EM003 = &
      "Memory allocation error: ia_Work in LAD"
INTEGER, PARAMETER :: i_REGSRMOD_ER004 = 70004
CHARACTER (LEN=*), PARAMETER :: ch_REGSRMOD_EM004 = &
      "Memory allocation error: da_DataSave in LAD"
INTEGER, PARAMETER :: i_REGSRMOD_ER005 = 70005
CHARACTER (LEN=*), PARAMETER :: ch_REGSRMOD_EM005 = &
      "Memory allocation error: da_DataRed,da_DataSaveRed,&
      &da_DepVarValsRed,da_ResSaveRed,da_BetasRed1st in LAD"
INTEGER, PARAMETER :: i_REGSRMOD_ER006 = 70006
CHARACTER (LEN=*), PARAMETER :: ch_REGSRMOD_EM006 = &
      "Memory alocation error: da_B,da_E,da_YH,da_X,da_C in LAD"
INTEGER, PARAMETER :: i_REGSRMOD_ER007 = 70007
CHARACTER (LEN=*), PARAMETER :: ch_REGSRMOD_EM007 = &
      "Memory allocation error da_RedA,da_ResRed1st,da_DepVarValsRed,da_CRed in &
      &LAD"
INTEGER, PARAMETER :: i_REGSRMOD_ER008 = 70008
CHARACTER (LEN=*), PARAMETER :: ch_REGSRMOD_EM008 = &
      "Memory allocation error: da_B,da_Y,da_C in LAD"
INTEGER, PARAMETER :: i_REGSRMOD_ER009 = 70009
CHARACTER (LEN=*), PARAMETER :: ch_REGSRMOD_EM009 = &
      "Memory allocation error: da_RedA,da_ResRed1st,da_DepVarValsRed,da_CRed in &
      &LAD"
INTEGER, PARAMETER :: i_REGSRMOD_ER010 = 70010
CHARACTER (LEN=*), PARAMETER :: ch_REGSRMOD_EM010 = &
      "Memory allocation error: da_Betas1st in LAD"
INTEGER, PARAMETER :: i_REGSRMOD_ER011 = 70011
CHARACTER (LEN=*), PARAMETER :: ch_REGSRMOD_EM011 = &
      "Problem creating LAD SAVE file in LSQSAVE: ""$1"""
INTEGER, PARAMETER :: i_REGSRMOD_ER018 = 70018
CHARACTER (LEN=*), PARAMETER :: ch_REGRSMOD_EM018 = &
      "Problem occurred in RQBR!"
CHARACTER (LEN=*), PARAMETER :: ch_REGRSMOD_EM019 = &
      "Regression mode unassigned in LAD12, program error"
CHARACTER (LEN=*), PARAMETER :: ch_REGRSMOD_EM020 = &
      "Perfect fit, zero residuals in model.$n&
      &We can't test this any further."
CHARACTER (LEN=*), PARAMETER :: ch_REGRSMOD_EM021 = &
      "Perfect fit, zero residuals in reduced model.$n&
      &We can't test this any further."
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
!  Can't allocate HatMatFull, BTemp in GETHATDIAG
!=============================================================================!
INTERFACE hsort    ! generic heap sort
   MODULE PROCEDURE dhsort, &  ! <File: "jonsmodule.f90: dhsort">
                   chhsort, &  ! <File: "jonsmodule.f90: chhsort">
                    ihsort, &  ! <File: "jonsmodule.f90: ihsort">
                  dhsort2d     ! <File: "jonsmodule.f90: dhsort2d">
END INTERFACE
INTERFACE lf_Equals   ! generic compare function
   MODULE PROCEDURE lf_DEquals, & ! <File: "jonsmodule.f90: lf_DEquals">
                    lf_REquals, & ! <File: "jonsmodule.f90: lf_REquals">
                    lf_CEquals, & ! <File: "jonsmodule.f90: lf_CEquals">
                    lf_IEquals, & ! <File: "jonsmodule.f90: lf_IEquals">
                    lf_LEquals    ! <File: "jonsmodule.f90: lf_LEquals">
END INTERFACE
CONTAINS
! Contains some general routines I use frequently. - JDR

!=============================================================================!

SUBROUTINE dhsort(da_Array, N)
!___Imported Parameters and Variables:
!__(none)__
!___Imported Procedures:
!__(none)__
IMPLICIT NONE
!___Dummy Arguments:
! REAL (KIND(0D0)), INTENT(INOUT) :: da_Array(:)
REAL (KIND(0D0)), INTENT(INOUT) :: da_Array(*)
INTEGER,          INTENT(IN)    :: N
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     SUBROUTINE hsort
! DESCRIPTION:
!     Perform a heap sort of da_Array array of REAL (KIND(0D0)) numbers.
!     NOTE: We explicitly provide dummy argument N because sometimes we
!        call this to sort a subset of N elements of the array from a
!        position provided in the call of the subroutine.
!        e.g., in codoset, to sort the block values within each group:
!           CALL hsort(da_BlockV(i_Start:), i_GpCnt)
! LANGUAGE:
!     Fortran 90/95
! AUTHOR:
!     Jon D. Richards, Operations Research Analyst    jon_richards@usgs.gov
!     Fort Collins Science Center                  http://www.fort.usgs.gov
!     U.S. Geological Survey          -          Biological Resources Division
! INVOKED BY:
!     codoset     <File: "docover.f90: codoset"> module
!     runksgf     <File: "docover.f90: runksgf"> module
! INVOKES:
!     -
! FILES:
!     -
! DEVICES:
!     -
! MODIFICATION HISTORY:
!   CODE HISTORY:
!     checked May 91 - JDR
!     Mod - to Lahey F77L compiler Oct 94 - JDR
!     Mod - to Lahey LF90 Fortran 90 compiler Oct/Nov 94 - JDR
!     Mod - Dec 95 - Lahey LF90 - Virtual Memory
!     Mod - updated logic, free form, double precision,
!           modules, binary files and style - JDR  Nov 1998
!     Mod - Changed from assumed shape to assumed size arrays, this is
!           much faster - JDR  29 Oct 2001
!   COMMENT HISTORY:
!     May 91 - JDR
!     Jun 91 - JDR
!     Updated - JDR  Nov 1998
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!___Local Variables:
REAL (KIND(0D0)) :: d_Num
INTEGER :: i, j
INTEGER :: iL, iR
                                                  ! debug!
IF (N <= 1) RETURN   ! ... normal exit ... but pathologic
iL = N/2 + 1
iR = N
DO
   IF (iL > 1) THEN
      iL = iL - 1
      d_Num = da_Array(iL)
   ELSE
      d_Num = da_Array(iR)
      da_Array(iR) = da_Array(1)
      iR = iR - 1
      IF (iR == 1) THEN
         da_Array(1) = d_Num
         RETURN   ! ... normal exit ...
      END IF
   END IF
   i = iL
   j = iL + iL
   DO WHILE (j <= iR)
      IF (j < iR) THEN
         IF (da_Array(j) < da_Array(j+1)) j = j + 1
      END IF
      IF (d_Num < da_Array(j)) THEN
         da_Array(i) = da_Array(j)
         i = j
         j = j + j
      ELSE
         j = iR + 1
      END IF
      call rchkusr()
   END DO
   da_Array(i) = d_Num
END DO
END SUBROUTINE dhsort

!=============================================================================!

SUBROUTINE sort2ara(da_Array1, da_Array2, N)
!___Imported Parameters and Variables:
!__(none)__
!___Imported Procedures:
!__(none)__
IMPLICIT NONE
!___Dummy Arguments:
! REAL (KIND(0D0)), INTENT(INOUT) :: da_Array1(:)
! REAL (KIND(0D0)), INTENT(INOUT) :: da_Array2(:)
REAL (KIND(0D0)), INTENT(INOUT) :: da_Array1(*)
REAL (KIND(0D0)), INTENT(INOUT) :: da_Array2(*)
INTEGER,          INTENT(IN)    :: N
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     SUBROUTINE hsort
! DESCRIPTION:
!     Perform a heap sort of da_Array1 array of REAL (KIND(0D0)) numbers.
!     Keep second arrray da_Array2 in sorted order of first array.
!     NOTE: We explicitly provide dummy argument N because sometimes we
!        call this to sort a subset of N elements of the array from a
!        position provided in the call of the subroutine.
!        e.g., in codoset, to sort the block values within each group:
!           CALL hsort(da_BlockV(i_Start:), i_GpCnt)
! LANGUAGE:
!     Fortran 90/95
! AUTHOR:
!     Jon D. Richards, Operations Research Analyst    jon_richards@usgs.gov
!     Fort Collins Science Center                  http://www.fort.usgs.gov
!     U.S. Geological Survey          -          Biological Resources Division
! INVOKED BY:
!     codoset     <File: "docover.f90: codoset"> module
!     runksgf     <File: "docover.f90: runksgf"> module
! INVOKES:
!     -
! FILES:
!     -
! DEVICES:
!     -
! MODIFICATION HISTORY:
!   CODE HISTORY:
!     Updated - JDR  17 May 2000
!     Mod - Changed from assumed shape to assumed size arrays, this is
!           much faster - JDR  29 Oct 2001
!   COMMENT HISTORY:
!     Updated - JDR  17 May 2000
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!___Local Variables:
REAL (KIND(0D0)) :: d_Num1, d_Num2
INTEGER :: i, j
INTEGER :: iL, iR
                                                       ! debug!
IF (N <= 1) RETURN   ! ... normal exit ... but pathologic
iL = N/2 + 1
iR = N
DO
   IF (iL > 1) THEN
      iL = iL - 1
      d_Num1 = da_Array1(iL)
      d_Num2 = da_Array2(iL)
   ELSE
      d_Num1 = da_Array1(iR)
      d_Num2 = da_Array2(iR)
      da_Array1(iR) = da_Array1(1)
      da_Array2(iR) = da_Array2(1)
      iR = iR - 1
      IF (iR == 1) THEN
         da_Array1(1) = d_Num1
         da_Array2(1) = d_Num2
         RETURN   ! ... normal exit ...
      END IF
   END IF
   i = iL
   j = iL + iL
   DO WHILE (j <= iR)
      IF (j < iR) THEN
         IF (da_Array1(j) < da_Array1(j+1)) j = j + 1
      END IF
      IF (d_Num1 < da_Array1(j)) THEN
         da_Array1(i) = da_Array1(j)
         da_Array2(i) = da_Array2(j)
         i = j
         j = j + j
      ELSE
         j = iR + 1
      END IF
      call rchkusr()
   END DO
   da_Array1(i) = d_Num1
   da_Array2(i) = d_Num2
END DO
END SUBROUTINE sort2ara

!=============================================================================!

SUBROUTINE chhsort(cha_Array, N)
!___Imported Parameters and Variables:
!__(none)__
!___Imported Procedures:
!__(none)__
IMPLICIT NONE
!___Dummy Arguments:
! CHARACTER (LEN=*), INTENT(INOUT) :: cha_Array(:)
CHARACTER (LEN=*), INTENT(INOUT) :: cha_Array(*)
INTEGER,           INTENT(IN)    :: N
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     SUBROUTINE chhsort
! DESCRIPTION:
!     Heap sort of a character array cha_Array.
! LANGUAGE:
!     Fortran 90/95
! AUTHOR:
!     Jon D. Richards, Operations Research Analyst    jon_richards@usgs.gov
!     Fort Collins Science Center                  http://www.fort.usgs.gov
!     U.S. Geological Survey          -          Biological Resources Division
! INVOKED BY:
!     -
! MODIFICATION HISTORY:
!   CODE HISTORY:
!     checked May 91 - JDR
!     Jan 93: changed to sort CHARACTER array (original was real)
!     Mod - Dec 95 - Lahey LF90 - Virtual Memory
!     Mod - updated logic, free form, double precision,
!           modules, binary files and style - JDR  Nov 1998
!     Mod - changed to allow assumed-shape dummy argument array of
!           assumed-length character strings. - JDR  26 Jan 1999
!     Mod - Changed from assumed shape to assumed size arrays, this is
!           much faster - JDR  29 Oct 2001
!   COMMENT HISTORY:
!     May 91 - JDR
!     Jun 91 - JDR
!     Updated - JDR  Nov 1998
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!___Local Variables:
INTEGER :: i, j
INTEGER :: iL, iR
CHARACTER (LEN=LEN(cha_Array)) :: ch_String
!___Intrinsic Procedures:
INTRINSIC :: LLT
!___Executable Statements:
IF (N <= 1) RETURN   ! ... normal exit ... (pathologic)
iL = N/2 + 1
iR = N
DO
   IF (iL > 1) THEN
      iL = iL - 1
      ch_String = cha_Array(iL)
   ELSE
      ch_String = cha_Array(iR)
      cha_Array(iR) = cha_Array(1)
      iR = iR - 1
      IF (iR == 1) THEN
         cha_Array(1) = ch_String
         RETURN   ! ... normal exit ...
      END IF
   END IF
   i = iL
   j = iL + iL
   DO WHILE (j <= iR)
      IF (j < iR) THEN
!lex            IF (cha_Array(J) < cha_Array(J+1)) J = J + 1
         IF (LLT(cha_Array(j),cha_Array(j+1))) j = j + 1
      END IF
!lex         IF (ch_String < cha_Array(J)) THEN
      IF (LLT(ch_String,cha_Array(j))) THEN
         cha_Array(i) = cha_Array(j)
         i = j
         j = j + j
      ELSE
         j = iR + 1
      END IF
      call rchkusr()
   END DO
   cha_Array(i) = ch_String
END DO
END SUBROUTINE chhsort

!=============================================================================!

SUBROUTINE ihsort(ia_Array, N)
!___Imported Parameters and Variables:
!__(none)__
!___Imported Procedures:
!__(none)__
IMPLICIT NONE
!___Dummy Arguments:
! INTEGER, INTENT(INOUT) :: ia_Array(:)
INTEGER, INTENT(INOUT) :: ia_Array(*)
INTEGER, INTENT(IN)    :: N
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     SUBROUTINE ihsort
! DESCRIPTION:
!     Perform a heap sort of ia_Array array of integer numbers.
!     NOTE: We explicitly provide dummy argument N because sometimes we
!        call this to sort a subset of N elements of the array from a
!        position provided in the call of the subroutine.
!           e.g., in codoset, to sort the block values within each group:
!               CALL hsort(iaBlockV(i_Start:), i_GpCnt)
! LANGUAGE:
!     Fortran 90/95
! AUTHOR:
!     Jon D. Richards, Operations Research Analyst    jon_richards@usgs.gov
!     Fort Collins Science Center                  http://www.fort.usgs.gov
!     U.S. Geological Survey          -          Biological Resources Division
! INVOKED BY:
!     ... (any) ...
! INVOKES:
!     -
! FILES:
!     -
! DEVICES:
!     -
! MODIFICATION HISTORY:
!   CODE HISTORY:
!     checked May 91 - JDR
!     Mod - to Lahey F77L compiler Oct 94 - JDR
!     Mod - to Lahey LF90 Fortran 90 compiler Oct/Nov 94 - JDR
!     Mod - Dec 95 - Lahey LF90 - Virtual Memory
!     Mod - updated logic, free form, double precision,
!           modules, binary files and style - JDR  Nov 1998
!     Mod - to integer sort for generic procedure. - JDR  18 Feb 1999
!     Mod - Changed from assumed shape to assumed size arrays, this is
!           much faster - JDR  29 Oct 2001
!   COMMENT HISTORY:
!     May 91 - JDR
!     Jun 91 - JDR
!     Updated - JDR  Nov 1998
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!___Local Variables:
INTEGER :: i, j
INTEGER :: iL, iR
INTEGER :: i_Num
!___Intrinsic Procedures:
!__(none)__
!___Executable Statements:
IF (N <= 1) RETURN   ! ... normal exit ... (pathologic)
iL = N/2 + 1
iR = N
DO
   IF (iL > 1) THEN
      iL = iL - 1
      i_Num = ia_Array(iL)
   ELSE
      i_Num = ia_Array(iR)
      ia_Array(iR) = ia_Array(1)
      iR = iR - 1
      IF (iR == 1) THEN
         ia_Array(1) = i_Num
         RETURN   ! ... normal exit ...
      END IF
   END IF
   i = iL
   j = iL + iL
   DO WHILE (j <= iR)
      IF (j < iR) THEN
         IF (ia_Array(j) < ia_Array(j+1)) j = j + 1
      END IF
      IF (i_Num < ia_Array(j)) THEN
         ia_Array(i) = ia_Array(j)
         i = j
         j = j + j
      ELSE
         j = iR + 1
      END IF
      call rchkusr()
   END DO
   ia_Array(i) = i_Num
END DO
END SUBROUTINE ihsort

!=============================================================================!

SUBROUTINE dhsort2d(da_Array, jn, iD, k)
!___Imported Parameters and Variables:
!__(none)__
!___Imported Procedures:
!__(none)__
IMPLICIT NONE
!___Dummy Arguments:
INTEGER,       INTENT(IN)       :: jn
INTEGER,       INTENT(IN)       :: iD
INTEGER,       INTENT(IN)       :: k
REAL (KIND(0D0)), INTENT(INOUT) :: da_Array(jn,iD)
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     SUBROUTINE dhsort2d
! DESCRIPTION:
!     Sort 2-dimensional array da_Array, 2nd index remains constant.
!      !---------------------------------------------------------!
!      !  older version (derived from Mielke's inline version):  !
!      !  nb: This version is not so good because of automatic   !
!      !      array da_X, whose size is determined (probably     !
!      !      incorrectly in this case) by dummy arguments. New  !
!      !      version has assumed shape array, da_Array.         !
!      !   subroutine hsortd2(da_X, J0, iD, I)                   !
!      !   ... dummy arguments ...                               !
!      !   INTEGER, INTENT(IN) :: J0                             !
!      !   INTEGER, INTENT(IN) :: iD                             !
!      !   INTEGER, INTENT(IN) :: I                              !
!      !   REAL (KIND(0D0)), INTENT(INOUT) :: da_X(J0,iD)        !
!      !   !___Local Variables:                                  !
!      !   INTEGER :: II, J, K, L                                !
!      !   REAL (KIND(0D0)) :: d_W                               !
!      !   !___Executable Statements:                            !
!      !         J = J0                                          !
!      !         K = J/2 + 1                                     !
!      !   10    K = K - 1                                       !
!      !         d_W = da_X(K,I)                                 !
!      !         GOTO 30                                         !
!      !   20    d_W = da_X(J,I)                                 !
!      !         da_X(J,I) = da_X(1,I)                           !
!      !         J = J - 1                                       !
!      !   30    II = K                                          !
!      !   40    L = 2*II                                        !
!      !         IF (L-J) 50,60,70                               !
!      !   50    IF (da_X(L+1,I) >= da_X(L,I)) L = L + 1         !
!      !   60    IF (d_W >= da_X(L,I)) GOTO 70                   !
!      !         da_X(II,I) = da_X(L,I)                          !
!      !         II = L                                          !
!      !         GOTO 40                                         !
!      !   70    da_X(II,I) = d_W                                !
!      !         IF (K > 1) GOTO 10                              !
!      !         IF (J >= 2) GOTO 20                             !
!      !   RETURN                                                !
!      !---------------------------------------------------------!
! LANGUAGE:
!     Fortran 90/95
! AUTHOR:
!     Jon D. Richards, Operations Research Analyst    jon_richards@usgs.gov
!     Fort Collins Science Center                  http://www.fort.usgs.gov
!     U.S. Geological Survey          -          Biological Resources Division
! INVOKED BY:
!     ... (any) ...
! INVOKES:
!     -
! FILES:
!     -
! DEVICES:
!     -
! MODIFICATION HISTORY:
!   CODE HISTORY:
!     Written - JDR  18 Feb 1999
!   COMMENT HISTORY:
!     Updated - JDR  18 Feb 1999
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!___Local Variables:
REAL (KIND(0D0)) :: d_Num
INTEGER :: i, j, N
INTEGER :: iL, iR
!___Intrinsic Procedures:
!__(none)__
!___Executable Statements:
N =jn
IF (N <= 1) RETURN   ! ... normal exit ... (pathologic)
iL = N/2 + 1
iR = N
DO
   IF (iL > 1) THEN
      iL = iL - 1
      d_Num = da_Array(iL,k)
   ELSE
      d_Num = da_Array(iR,k)
      da_Array(iR,k) = da_Array(1,k)
      iR = iR - 1
      IF (iR == 1) THEN
         da_Array(1,k) = d_Num
         RETURN   ! ... normal exit ...
      END IF
   END IF
   i = iL
   j = iL + iL
   DO WHILE (j <= iR)
      IF (j < iR) THEN
         IF (da_Array(j,k) < da_Array(j+1,k)) j = j + 1
      END IF
      IF (d_Num < da_Array(j,k)) THEN
         da_Array(i,k) = da_Array(j,k)
         i = j
         j = j + j
      ELSE
         j = iR + 1
      END IF
      call rchkusr()
   END DO
   da_Array(i,k) = d_Num
END DO
END SUBROUTINE dhsort2d

!=============================================================================!

SUBROUTINE hsort3(da_Array1, da_Array2, ia_Array, n)
!___Imported Parameters and Variables:
!__(none)__
!___Imported Procedures:
!__(none)__
IMPLICIT NONE
!___Dummy Arguments:
REAL (KIND(0D0)), INTENT(INOUT) :: da_Array1(:)
REAL (KIND(0D0)), INTENT(INOUT) :: da_Array2(:)
INTEGER,          INTENT(INOUT) :: ia_Array(:)
INTEGER,          INTENT(IN)    :: n
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     SUBROUTINE hsort3
! DESCRIPTION:
!     Do heapsort on da_Array1, keep other two arrays in sorted order.
! LANGUAGE:
!     Fortran 90/95
! AUTHOR:
!     Jon D. Richards, Operations Research Analyst    jon_richards@usgs.gov
!     Fort Collins Science Center                  http://www.fort.usgs.gov
!     U.S. Geological Survey          -          Biological Resources Division
! INVOKED BY:
!     qsortndx <File: "domrpp.f90: qsortndx"> module
! INVOKES:
!     -
! FILES:
!     -
! DEVICES:
!     -
! MODIFICATION HISTORY:
!   CODE HISTORY:
!     checked May 91 - JDR
!     Mod - to Lahey F77L compiler Oct 94 - JDR
!     Mod - to Lahey LF90 Fortran 90 compiler Oct/Nov 94 - JDR
!     Mod - Dec 95 - Lahey LF90 - Virtual Memory
!     Mod - updated logic, free form, double precision,
!           modules, binary files and style - JDR  Nov 1998
!   COMMENT HISTORY:
!     May 91 - JDR
!     Updated - JDR  Nov 1998
!
! There is nothing more difficult to take in hand, more perilous to
! conduct, or more uncertain in its success, than to take the lead in
! the introduction of a new ORDER of things.
! (Machiavelli, The Prince, 1513)
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!___Local Variables:
REAL (KIND(0D0)) :: d_Num1, d_Num2
INTEGER :: i, j
INTEGER :: iL, iR
INTEGER :: i_Num
LOGICAL :: l_Use2ndArray
!___Intrinsic Procedures:
INTRINSIC :: UBOUND
                                                               ! debug!
l_Use2ndArray = (UBOUND(da_Array2,1) == UBOUND(da_Array1,1))
IF (n <= 1) RETURN   ! ... normal exit ...
iL = n/2 + 1
iR = n
DO
   IF (iL > 1) THEN
      iL = iL - 1
      d_Num1 = da_Array1(iL)
      IF (l_Use2ndArray) d_Num2 = da_Array2(iL)
      i_Num  = ia_Array(iL)
   ELSE
      d_Num1 = da_Array1(iR)
      IF (l_Use2ndArray) d_Num2 = da_Array2(iR)
      i_Num  = ia_Array(iR)
      da_Array1(iR) = da_Array1(1)
      IF (l_Use2ndArray) da_Array2(iR) = da_Array2(1)
      ia_Array(iR) = ia_Array(1)
      iR = iR - 1
      IF (iR == 1) THEN
         da_Array1(1) = d_Num1
         IF (l_Use2ndArray) da_Array2(1) = d_Num2
         ia_Array(1)  = i_Num
         RETURN   ! ... normal exit ...
      END IF
   END IF
   i = iL
   j = iL + iL
   DO WHILE (j <= iR)
      IF (j < iR) THEN
         IF (da_Array1(j) < da_Array1(j+1)) j = j + 1
      END IF
      IF (d_Num1 < da_Array1(j)) THEN
         da_Array1(i) = da_Array1(j)
         IF (l_Use2ndArray) da_Array2(i) = da_Array2(j)
         ia_Array(i)  = ia_Array(j)
         i = j
         j = j + j
      ELSE
         j = iR + 1
      END IF
      call rchkusr()
   END DO
   da_Array1(i) = d_Num1
   IF (l_Use2ndArray) da_Array2(i) = d_Num2
   ia_Array(i)  = i_Num
END DO
END SUBROUTINE hsort3

!=============================================================================!

SUBROUTINE hsort2(da_Array, ia_Array, n)
!___Imported Parameters and Variables:
!__(none)__
!___Imported Procedures:
!__(none)__
IMPLICIT NONE
!___Dummy Arguments:
! REAL (KIND(0D0)), INTENT(INOUT) :: da_Array(:)
! INTEGER,          INTENT(INOUT) :: ia_Array(:)
REAL (KIND(0D0)), INTENT(INOUT) :: da_Array(*)
INTEGER,          INTENT(INOUT) :: ia_Array(*)
INTEGER,          INTENT(IN)    :: n
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     SUBROUTINE hsort2
! DESCRIPTION:
!     Do heapsort on da_Array, keep integer array ia_Array in sorted order.
! LANGUAGE:
!     Fortran 90/95
! AUTHOR:
!     Jon D. Richards, Operations Research Analyst    jon_richards@usgs.gov
!     Fort Collins Science Center                  http://www.fort.usgs.gov
!     U.S. Geological Survey          -          Biological Resources Division
! INVOKED BY:
!     rmedq1   <File: "medqmod.f90: rmedq1"> module
!     rmedq    <File: "medqmod.f90: rmedq">  module
!     spdoset  <File: "domrsp.f90: spdoset"> module
! INVOKES:
!     -
! FILES:
!     -
! DEVICES:
!     -
! MODIFICATION HISTORY:
!   CODE HISTORY:
!     created - from hsort3 code - JDR  14 Jul 2000
!     Mod - Changed from assumed shape to assumed size arrays, this is
!           much faster - JDR  29 Oct 2001
!   COMMENT HISTORY:
!     Updated - JDR  14 Jul 2000
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!___Local Variables:
REAL (KIND(0D0)) :: d_Num
INTEGER :: i, j
INTEGER :: iL, iR
INTEGER :: i_Num
                                                              ! debug!
IF (n <= 1) RETURN   ! ... normal exit ...
iL = n/2 + 1
iR = n
DO
   IF (iL > 1) THEN
      iL = iL - 1
      d_Num = da_Array(iL)
      i_Num = ia_Array(iL)
   ELSE
      d_Num = da_Array(iR)
      i_Num = ia_Array(iR)
      da_Array(iR) = da_Array(1)
      ia_Array(iR) = ia_Array(1)
      iR = iR - 1
      IF (iR == 1) THEN
         da_Array(1) = d_Num
         ia_Array(1) = i_Num
         RETURN   ! ... normal exit ...
      END IF
   END IF
   i = iL
   j = iL + iL
   DO WHILE (j <= iR)
      IF (j < iR) THEN
         IF (da_Array(j) < da_Array(j+1)) j = j + 1
      END IF
      IF (d_Num < da_Array(j)) THEN
         da_Array(i) = da_Array(j)
         ia_Array(i) = ia_Array(j)
         i = j
         j = j + j
      ELSE
         j = iR + 1
      END IF
      call rchkusr()
   END DO
   da_Array(i) = d_Num
   ia_Array(i) = i_Num
END DO
END SUBROUTINE hsort2

!=============================================================================!

SUBROUTINE hsorti2(da_Array, ia_Array, n)
!___Imported Parameters and Variables:
!__(none)__
!___Imported Procedures:
!__(none)__
IMPLICIT NONE
!___Dummy Arguments:
! REAL (KIND(0D0)), INTENT(INOUT) :: da_Array(:)
! INTEGER,          INTENT(INOUT) :: ia_Array(:)
REAL (KIND(0D0)), INTENT(INOUT) :: da_Array(*)
INTEGER,          INTENT(INOUT) :: ia_Array(*)
INTEGER,          INTENT(IN)    :: n
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     SUBROUTINE hsorti2
! DESCRIPTION:
!     Do heapsort on integer array ia_Array, keep da_Array in sorted order.
! LANGUAGE:
!     Fortran 90/95
! AUTHOR:
!     Jon D. Richards, Operations Research Analyst    jon_richards@usgs.gov
!     Fort Collins Science Center                  http://www.fort.usgs.gov
!     U.S. Geological Survey          -          Biological Resources Division
! INVOKED BY:
!     rmedq1   <File: "medqmod.f90: rmedq1"> module
!     rmedq    <File: "medqmod.f90: rmedq">  module
!     spdoset  <File: "domrsp.f90: spdoset"> module
! INVOKES:
!     -
! FILES:
!     -
! DEVICES:
!     -
! MODIFICATION HISTORY:
!   CODE HISTORY:
!     created - from hsort3 code - JDR  14 Jul 2000
!     Mod - Changed from assumed shape to assumed size arrays, this is
!           much faster - JDR  29 Oct 2001
!   COMMENT HISTORY:
!     Updated - JDR  14 Jul 2000
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!___Local Variables:
REAL (KIND(0D0)) :: d_Num
INTEGER :: i, j
INTEGER :: iL, iR
INTEGER :: i_Num
                                                              ! debug!
IF (n <= 1) RETURN   ! ... normal exit ...
iL = n/2 + 1
iR = n
DO
   IF (iL > 1) THEN
      iL = iL - 1
      d_Num = da_Array(iL)
      i_Num = ia_Array(iL)
   ELSE
      d_Num = da_Array(iR)
      i_Num = ia_Array(iR)
      da_Array(iR) = da_Array(1)
      ia_Array(iR) = ia_Array(1)
      iR = iR - 1
      IF (iR == 1) THEN
         da_Array(1) = d_Num
         ia_Array(1) = i_Num
         RETURN   ! ... normal exit ...
      END IF
   END IF
   i = iL
   j = iL + iL
   DO WHILE (j <= iR)
      IF (j < iR) THEN
         IF (ia_Array(j) < ia_Array(j+1)) j = j + 1
      END IF
      IF (i_Num < ia_Array(j)) THEN
         da_Array(i) = da_Array(j)
         ia_Array(i) = ia_Array(j)
         i = j
         j = j + j
      ELSE
         j = iR + 1
      END IF
      call rchkusr()
   END DO
   da_Array(i) = d_Num
   ia_Array(i) = i_Num
END DO
END SUBROUTINE hsorti2

!=============================================================================!

FUNCTION lf_REquals(a, b) RESULT(l_Result)
!___Imported Parameters and Variables:
!__(none)__
!___Imported Procedures:
!__(none)__
IMPLICIT NONE
!___Dummy Arguments:
REAL (KIND(0.0)), INTENT(IN) :: a
REAL (KIND(0.0)), INTENT(IN) :: b
!___Function Return Result:
LOGICAL :: l_Result
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
! Reasonably safe compare of two default real single precision numbers.
! From Lahey LF90 Language Reference p.119
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!___Local Variables:
REAL (KIND(0.0)) :: eps
!___Intrinsic Procedures:
INTRINSIC :: ABS, EPSILON, TINY
!___Executable Statements:
eps = ABS(a) * EPSILON(a) ! scale epsilon
IF (eps == 0) THEN
   eps = TINY(a)          ! If eps is underflowed to zero
                          ! then use a very small positive
                          ! value for epsilon.
END IF
IF (ABS(a-b) > eps) THEN
   l_Result = .FALSE.     ! Not equal if difference > eps.
ELSE
   l_Result = .TRUE.      ! Equal otherwise.
END IF
RETURN   ! ... normal exit ...
END FUNCTION lf_REquals

!=============================================================================!

FUNCTION lf_DEquals(a, b) RESULT(l_Result)
!___Imported Parameters and Variables:
!__(none)__
!___Imported Procedures:
!__(none)__
IMPLICIT NONE
!___Dummy Arguments:
REAL (KIND(0D0)), INTENT(IN) :: a
REAL (KIND(0D0)), INTENT(IN) :: b
!___Function Return Result:
LOGICAL :: l_Result
! .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .
! Reasonably safe compare of two default double precision real numbers.
! From Lahey LF90 Language Reference p.119
! .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .
!___Local Variables:
REAL (KIND(0D0)) :: eps
!___Intrinsic Procedures:
INTRINSIC :: ABS, DBLE, EPSILON, HUGE, REAL, TINY
!___Executable Statements:
IF (ABS(a-b) >= 1.0D-6) THEN ! obviously not equal
!
   l_Result = .FALSE.
   RETURN
END IF

IF (a > DBLE(HUGE(1.0E0)) .OR. b > DBLE(HUGE(1.0E0))) THEN
   eps = TINY(REAL(a))
ELSE
   eps = ABS(REAL(a)) * EPSILON(REAL(a)) ! scale epsilon

END IF
IF (eps == 0) THEN
   eps = TINY(REAL(a))    ! If eps is underflowed to zero
                          ! then use a very small positive
                          ! value for epsilon
END IF

IF (ABS(REAL(a-b)) > eps) THEN
   l_Result = .FALSE.     ! not equal if difference>eps
ELSE
   l_Result = .TRUE.      ! equal otherwise
END IF

RETURN   ! ... normal exit ...
END FUNCTION lf_DEquals

!=============================================================================!

FUNCTION lf_CEquals(a, b) RESULT(l_Result)
!___Imported Parameters and Variables:
!__(none)__
!___Imported Procedures:
!__(none)__
IMPLICIT NONE
!___Dummy Arguments:
CHARACTER (LEN=*), INTENT(IN) :: a
CHARACTER (LEN=*), INTENT(IN) :: b
!___Function Return Result:
LOGICAL :: l_Result
!___Intrinsic Procedures:
   INTRINSIC ADJUSTL
!___Executable Statements:
IF (ADJUSTL(a) == ADJUSTL(b)) THEN
   l_Result = .TRUE.
ELSE
   l_Result = .FALSE.
END IF
END FUNCTION lf_CEquals

!=============================================================================!

FUNCTION lf_IEquals(a, b) RESULT(l_Result)
!___Imported Parameters and Variables:
!__(none)__
!___Imported Procedures:
!__(none)__
IMPLICIT NONE
!___Dummy Arguments:
INTEGER, INTENT(IN) :: a
INTEGER, INTENT(IN) :: b
!___Function Return Result:
LOGICAL :: l_Result
!___Executable Statements:
IF (a == b) THEN
   l_Result = .TRUE.
ELSE
   l_Result = .FALSE.
END IF
END FUNCTION lf_IEquals

!=============================================================================!

FUNCTION lf_LEquals(l_a, l_b) RESULT(l_Result)
!___Imported Parameters and Variables:
!__(none)__
!___Imported Procedures:
!__(none)__
IMPLICIT NONE
!___Dummy Arguments:
LOGICAL, INTENT(IN) :: l_a
LOGICAL, INTENT(IN) :: l_b
!___Function Return Result:
LOGICAL :: l_Result
!___Executable Statements:
IF (l_a) THEN
   IF (l_b) THEN
      l_Result = .TRUE.
   ELSE
      l_Result = .FALSE.
   END IF
ELSE
   IF (l_b) THEN
      l_Result = .FALSE.
   ELSE
      l_Result = .TRUE.
   END IF
END IF
END FUNCTION lf_LEquals


SUBROUTINE errhand(ch_MyMsg, &
					i_Code, &
                   ch_Msg, &
                   ch_S1,  &
                   ch_S2,  &
                   ch_S3,  &
                   ch_S4,  &
                   ch_S5,  &
                   ch_S6,  &
                   ch_S7,  &
                   ch_S8,  &
                   ch_S9,  &
                   l_CR)

!___Imported Parameters and Variables:
USE blcmnmod, ONLY: l_Console  !* <File: "blcmnmod.f90">
        ! ^-- Some global Blossom variables and parameters.
!___Imported Procedures:

IMPLICIT NONE
!___Dummy Arguments:
CHARACTER (LEN=*), OPTIONAL, INTENT(OUT) :: ch_MyMsg
INTEGER,           OPTIONAL, INTENT(IN) :: i_Code
CHARACTER (LEN=*), OPTIONAL, INTENT(IN) :: ch_Msg
CHARACTER (LEN=*), OPTIONAL, INTENT(IN) :: ch_S1
CHARACTER (LEN=*), OPTIONAL, INTENT(IN) :: ch_S2
CHARACTER (LEN=*), OPTIONAL, INTENT(IN) :: ch_S3
CHARACTER (LEN=*), OPTIONAL, INTENT(IN) :: ch_S4
CHARACTER (LEN=*), OPTIONAL, INTENT(IN) :: ch_S5
CHARACTER (LEN=*), OPTIONAL, INTENT(IN) :: ch_S6
CHARACTER (LEN=*), OPTIONAL, INTENT(IN) :: ch_S7
CHARACTER (LEN=*), OPTIONAL, INTENT(IN) :: ch_S8
CHARACTER (LEN=*), OPTIONAL, INTENT(IN) :: ch_S9
LOGICAL,           OPTIONAL, INTENT(IN) :: l_CR

!Leftovers from blcmnod
!INTEGER, PARAMETER :: i_LEN_MY_EMSG = 65536 ! needs to be this big for output of var list in dostatus.
!CHARACTER (LEN=i_LEN_MY_EMSG) :: ch_MyMsg
! .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .
!     SUBROUTINE errhand
! DESCRIPTION:
!     Handle error code iEr with appropriate message to console.
! NOTE: WE may want to add the additional functionality of formatting numbers
!       with the convention $i and $d for our integers and double precision
!       numbers.  It here and do the
!       necessary TRIM( ) rather than do this in the calling procedure. This
!       would help a lot in the size of the main program. The problem for us
!       here with Fortran is having enough integer and real dummy arguments
!       (albeit they are optionals) for an adequate interface.  The strings
!       would still pass as S1...S9, but a simple $i or $d for each number
!       would be sufficient for an equal number of integer and double arguments.

!___Procedure Parameters:
INTEGER,           PARAMETER :: i_LEN_ERR_MSG = 145
CHARACTER (LEN=*), PARAMETER :: ch_FMTN       = "(A)"
! CHARACTER (LEN=*), PARAMETER :: ch_FMT1       = "(1x,A)"
!___Local Variables:
INTEGER :: i
INTEGER :: i_Len, i_MyLen
LOGICAL :: l_ToScreen, l_ToOutFile, l_Advance
CHARACTER (LEN=i_LEN_ERR_MSG) :: ch_IOMsg
! CHARACTER (LEN=6) :: ch_FMT
CHARACTER (LEN=1) :: ch_Ch
!___Intrinsic Procedures:
INTRINSIC :: CHAR, LEN_TRIM, PRESENT, TRIM

l_Advance = .TRUE.

IF (PRESENT(ch_Msg)) THEN
   i_Len = LEN_TRIM(ch_Msg)
   ch_MyMsg = ""
   i_MyLen  = 0
   i = 1
   DO
      ch_Ch = ch_Msg(i:i)
!       IF (I==1) THEN
!          ch_FMT = ch_FMT1
!       ELSE
!          ch_FMT = ch_FMTN
!       END IF
      IF (ch_Ch /= "$") THEN
         ch_MyMsg = ch_MyMsg(1:i_MyLen+1)//ch_Ch
         i_MyLen  = i_MyLen + 1
      ELSE
         i = i + 1
         ch_Ch = ch_Msg(i:i)
         SELECT CASE (ch_Ch)
            CASE ("$")
               ch_MyMsg = ch_MyMsg(1:i_MyLen+1)//ch_Ch
               i_MyLen  = i_MyLen + 1
            CASE ("1")
               IF (PRESENT(ch_S1)) THEN
                  ch_MyMsg = ch_MyMsg(1:i_MyLen+1)//TRIM(ch_S1)
                  i_MyLen  = i_MyLen + LEN_TRIM(ch_S1)
               END IF
            CASE ("2")
               IF (PRESENT(ch_S2)) THEN
                  ch_MyMsg = ch_MyMsg(1:i_MyLen+1)//TRIM(ch_S2)
                  i_MyLen  = i_MyLen + LEN_TRIM(ch_S2)
               END IF
            CASE ("3")
               IF (PRESENT(ch_S3)) THEN
                  ch_MyMsg = ch_MyMsg(1:i_MyLen+1)//TRIM(ch_S3)
                  i_MyLen  = i_MyLen + LEN_TRIM(ch_S3)
               END IF
            CASE ("4")
               IF (PRESENT(ch_S4)) THEN
                  ch_MyMsg = ch_MyMsg(1:i_MyLen+1)//TRIM(ch_S4)
                  i_MyLen  = i_MyLen + LEN_TRIM(ch_S4)
               END IF
            CASE ("5")
               IF (PRESENT(ch_S5)) THEN
                  ch_MyMsg = ch_MyMsg(1:i_MyLen+1)//TRIM(ch_S5)
                  i_MyLen  = i_MyLen + LEN_TRIM(ch_S5)
               END IF
            CASE ("6")
               IF (PRESENT(ch_S6)) THEN
                  ch_MyMsg = ch_MyMsg(1:i_MyLen+1)//TRIM(ch_S6)
                  i_MyLen  = i_MyLen + LEN_TRIM(ch_S6)
               END IF
            CASE ("7")
               IF (PRESENT(ch_S7)) THEN
                  ch_MyMsg = ch_MyMsg(1:i_MyLen+1)//TRIM(ch_S7)
                  i_MyLen  = i_MyLen + LEN_TRIM(ch_S7)
               END IF
            CASE ("8")
               IF (PRESENT(ch_S8)) THEN
                  ch_MyMsg = ch_MyMsg(1:i_MyLen+1)//TRIM(ch_S8)
                  i_MyLen  = i_MyLen + LEN_TRIM(ch_S8)
               END IF
            CASE ("9")
               IF (PRESENT(ch_S9)) THEN
                  ch_MyMsg = ch_MyMsg(1:i_MyLen+1)//TRIM(ch_S9)
                  i_MyLen  = i_MyLen + LEN_TRIM(ch_S9)
               END IF
            CASE ("n") 
               ch_MyMsg = ch_MyMsg(1:i_MyLen+1)//CHAR(13)//CHAR(10)
               i_MyLen  = i_MyLen + 2
         END SELECT
      END IF
      i = i + 1
      IF (i > i_Len) EXIT
      call rchkusr()
   END DO
   IF (PRESENT(l_CR)) THEN
      IF (l_CR) THEN
         ch_MyMsg = ch_MyMsg(1:i_MyLen+1)//CHAR(13)//CHAR(10)
         i_MyLen  = i_MyLen + 2
      END IF
   ELSE
!?       ch_MyMsg = ch_MyMsg(1:i_MyLen+1)//CHAR(13)//CHAR(10)
!?       i_MyLen  = i_MyLen + 2
   END IF
   IF (PRESENT(l_CR)) THEN    ! //////////////????????????????????????? need this?
      IF (l_CR) THEN          ! //////////////????????????????????????? need this?
         l_Advance = .TRUE.   ! //////////////????????????????????????? need this?
      ELSE                    ! //////////////????????????????????????? need this?
         l_Advance = .FALSE.  ! //////////////????????????????????????? need this?
      END IF                  ! //////////////????????????????????????? need this?
   END IF                     ! //////////////????????????????????????? need this?

END IF



END SUBROUTINE errhand

SUBROUTINE getrseed(i_Seed)
!___Imported Parameters and Variables:
!__(none)__
!___Imported Procedures:
!__(none)__
IMPLICIT NONE
!___Dummy Arguments:
INTEGER, INTENT(OUT) :: i_Seed
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     SUBROUTINE getrseed
! DESCRIPTION:
!     Get random number seed (use system clock).
!     Gets clock ticks since midnight.
!     Used by coverage test (gsect1).

! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!___Local Variables:
INTEGER :: icm, icr ! Defined here for subroutine call but not used.
!___Intrinsic Procedures:
INTRINSIC :: SYSTEM_CLOCK
!___Executable Statements:
CALL SYSTEM_CLOCK(i_Seed, icr, icm)
RETURN   ! ... normal exit ...
END SUBROUTINE getrseed

!========================================================
FUNCTION getepsz(d_X, d_DefTol) RESULT(d_EPSZ)
!___Imported Parameters and Variables:
!__(none)__
!___Imported Procedures:
!__(none)__
IMPLICIT NONE
!___Dummy Arguments:
REAL (KIND(0D0)), INTENT(IN)           :: d_X
REAL (KIND(0D0)), INTENT(IN), OPTIONAL :: d_DefTol
!___Function Return Result:
REAL (KIND(0D0))                       :: d_EPSZ
! .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .
!     SUBROUTINE getepsz
! DESCRIPTION:
!     Get a good tolerance value for the magnitude of the number d_X.
!     If a problem occurs and a default is given then use it, else
!     use the DEFAULT_TOLERANCE here.

!___Procedure Parameters:
REAL (KIND(0D0)), PARAMETER :: d_DEFAULT_TOLERANCE  = 1.0D-12
! i_NUMB_SIGNIF_DIGITS is (the same as) PRECISION(d_X) for my processor.
INTEGER,          PARAMETER :: i_NUMB_SIGNIF_DIGITS = 15
!___Local Variables:
INTEGER :: ios
INTEGER :: i_Len, i_Mag, i_Posn1, i_Posn2
CHARACTER (LEN=25) :: ch_Target
CHARACTER (LEN=4)  :: ch_Mag
REAL (KIND(0D0)) :: d_absVal
!___Intrinsic Procedures:
INTRINSIC :: ADJUSTR, INDEX, LEN_TRIM, PRESENT
!___Executable Statements:

!=======================================         
!here's my code for figuring out the magnatude
!take the absolute value figure out the magnatude
!and use this minus the number of sig digits which is set at 15 I believe
d_absVal=d_X
if (d_X<0.0D0) then  
  d_absVal=-1*d_absVal
end if
if(d_absVal<1) then
  i_Mag=0
else if (d_absVal<10.000) then 
  i_Mag=1  
else if (d_absVal<100.000) then 
  i_Mag=2
else if (d_absVal<1000.000) then 
  i_Mag=3
else if (d_absVal<10000.000) then 
  i_Mag=4
else if (d_absVal<100000.000) then 
  i_Mag=5
else if (d_absVal<1000000.000) then 
  i_Mag=6    
else if (d_absVal<10000000.000) then 
  i_Mag=7
else if (d_absVal<100000000.000) then 
  i_Mag=8 
else 
  i_Mag=9       
end if

i_Mag  = i_Mag - i_NUMB_SIGNIF_DIGITS

d_EPSZ = 1.6D0*10.0D0**i_Mag
RETURN   ! ... normal return ...
END FUNCTION getepsz

FUNCTION lf_IsIn(ch_String, ch_SubString) RESULT(l_Result)
!___Imported Parameters and Variables:
!__(none)__
!___Imported Procedures:
!__(none)__
IMPLICIT NONE
!___Dummy Arguments:
CHARACTER (LEN=*), INTENT(IN) :: ch_String
CHARACTER (LEN=*), INTENT(IN) :: ch_SubString
!___Function Return Result:
LOGICAL :: l_Result
! .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .
! DESCRIPTION:
!     Return TRUE if substring is in string.
! LANGUAGE:
!     Fortran 90/95
! AUTHOR:
!     Jon D. Richards, Operations Research Analyst    jon_richards@usgs.gov
!     Fort Collins Science Center                  http://www.fort.usgs.gov
!     U.S. Geological Survey          -          Biological Resources Division
! INVOKED BY:

!     displad      <File: "dolad.f90: displad">         module
!     dispmrpp     <File: "domrsp.f90: dispmrpp">       module
!     docover      <File: "docover.f90: docover">       module
!     dohelp       <File: "blossomod.f90: dohelp">      module
!     domedq       <File: "domedq.f90: domedq">         module
!     domrpp       <File: "domrpp.f90: domrpp">         module
!     dose         <File: "blossomod.f90: dosave">      module
!     dosubmit     <File: "blossomod.f90: dosubmit">    module
!     douse        <File: "blossomod.f90: douse">       module
!     getcmd       <File: "blossomod.f90: getcmd">      module
!     getcmdgpvals <File: "mrauxmod.f90: getcmdgpvals"> module
!     getdiflb     <File: "loadrmod.f90: getdiflb">     module
!     getladsavfn  <File: "dolad.f90: getladsavfn">     module
!     getmqdistfn  <File: "domedq.f90: getmqdistfn">    module
!     getvarname   <File: "mrauxmod.f90: getvarname">   module
!     loaddat      <File: "blossom0d.f90: loaddat">     module
!     mqoption     <File: "domedq.f90: mqoption">       module
!     parsecmdlin  <File: "blossomod.f90: parsecmdlin"> module
! .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .
!___Local Variables:
!__(none)__
!___Intrinsic Procedures:
INTRINSIC :: INDEX
!___Executable Statements:
IF (INDEX(ch_String,ch_SubString) == 0) THEN
   l_Result = .FALSE.
ELSE
   l_Result = .TRUE.
END IF
END FUNCTION lf_IsIn
!=============================================================================!
END MODULE jonsmodule


!     ******************************************************************
!     invertmod.f90
!     Author: Jon D. Richards    U.S. Geological Survey
!     Last change: JDR 8/16/2007 5:06:47 PM
!     ******************************************************************
!#/WINTERACTER/ ! This Comment means it is for Windows (WINTERACTER) version only.             !#/WINTERACTER/
! This Comment means it is for Console (CON) version only.                     !#/CON/
!  INVERT   <File: "invrtmod.f90: invert">
!=============================================================================!

MODULE invertmod
IMPLICIT NONE
SAVE
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     MODULE invrtmod.f90 -
! DESCRIPTION:
!     Matrix inversion with check for singularity.
! LANGUAGE:
!     Fortran 90/95
! AUTHOR:
!     Jon D. Richards, Operations Research Analyst       jon_richards@usgs.gov
!     Fort Collins Science Center                     http://www.fort.usgs.gov
!     U.S. Geological Survey          -          Biological Resources Division
! MODIFICATION HISTORY:
!     Date Created:   28 Jun 1999
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
CONTAINS
!     INVERT   <File: "invrtmod.f90: invert">

!=============================================================================!

SUBROUTINE invert(i_N, da_W, iEr)
!___Imported Parameters and Variables:
USE blcmnmod, ONLY: d_ONE, d_ZERO, i_OK, i_ZERO
        ! ^-- Some global Blossom variables and parameters.
USE mrolamod, ONLY: i_MAT_SINGULARITY
        ! ^-- Some high-level Blossom parameters.
USE jonsmodule, ONLY: i_INVRTMOD_ER001, i_INVRTMOD_ER002
        ! ^-- Message handling, etc.
!___Imported Procedures
!<none>
IMPLICIT NONE
INTEGER :: i_CCode
!___Dummy Arguments:
INTEGER,          INTENT(IN)    :: i_N
REAL (KIND(0D0)), INTENT(INOUT) :: da_W(i_N,i_N)
INTEGER,          INTENT(OUT)   :: iEr
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     SUBROUTINE invert
! DESCRIPTION:
!     Routine from Dr. Paul Mielke, Colo. State Univ.
!        http://www.stat.colostate.edu/~mielke/mielkesoftware.html
! LANGUAGE:
!     Routine from Dr. Paul Mielke, Colo. State Univ.
!        http://www.stat.colostate.edu/~mielke/mielkesoftware.html
!     Fortran 90, with Lahey Computer Systems LF90 intrinsics extensions
! AUTHOR:
!     Jon D. Richards, Operations Research Analyst    jon_richards@usgs.gov
!     Fort Collins Science Center                  http://www.fort.usgs.gov
!     U.S. Geological Survey          -          Biological Resources Division
! MODIFICATION HISTORY:
!     Adapted to Fortran 90 - March 98 - Jon D Richards USGS.
!     Put into module; call Scientific Subroutine Library to
!     check for singularity or pivot near zero - JDR - Oct 1998.
!     Date Module Created:   28 Jun 1999
!     Mod - Replaced some loop constructs with intrinsic operations on
!           arrays or array assignments. This is faster. - JDR  14 Dec 1999
! INVOKED BY:
!     emrpp        <File: "mrppmod.f90: emrpp">         module
!     emvptmp      <File: "mrppmod.f90: emvptmp">       module
!     lsqredcalc   <File: "regrsmod.f90: lsqredcalc">   module
!     mrppdist     <File: "mrppmod.f90: mrppdist">      module
!     rslsqredcalc <File: "regrsmod.f90: rslsqredcalc"> module
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!___Local Variables:
REAL (KIND(0D0)), ALLOCATABLE :: da_SSLA(:,:) ! temp array
REAL (KIND(0D0)), ALLOCATABLE :: da_SSLVW(:)  ! temp array
REAL (KIND(0D0)) :: d_SSLEPSZ ! relative 0 pivot test, use SSL2 standard
REAL (KIND(0D0)) :: d_T
INTEGER, ALLOCATABLE :: ia_KC(:)
INTEGER, ALLOCATABLE :: ia_KP(:)
INTEGER, ALLOCATABLE :: ia_KR(:)
INTEGER, ALLOCATABLE :: ia_SSLIP(:) ! temp array
   ! Variables associated with Scientific Subroutine Library call
   ! to DALU where we find if we can invert the var/cov matrix:
INTEGER :: i_SSLK, i_SSLN, i_SSLIS, i_SSLICON  ! temp vars
INTEGER :: i, j, k, l, m
INTEGER :: ios
INTEGER :: i_IC, i_IR
!___Intrinsic Procedures:
INTRINSIC :: ABS, ALLOCATED
!___Executable Statements:
iEr = i_OK ! no problem yet
ALLOCATE(           &
      ia_KP(1:i_N), &
      ia_KR(1:i_N), &
      ia_KC(1:i_N), &
      STAT=iEr)
IF (iEr /= i_OK) THEN   ! could not allocate array space
!   ________________________________________________________
!  |                                                        |
!  | "Memory allocation error: ia_KP,ia_KR,ia_KC in INVERT" |
!  |________________________________________________________|
!
   i_CCode = iEr
   iEr = i_INVRTMOD_ER001
   IF (ALLOCATED(ia_KP)) DEALLOCATE(ia_KP, STAT=ios)
   IF (ALLOCATED(ia_KR)) DEALLOCATE(ia_KR, STAT=ios)
   IF (ALLOCATED(ia_KC)) DEALLOCATE(ia_KC, STAT=ios)
   RETURN   ! ... error exit ...
END IF
! call Scientific Subroutine Library to determine
! if this matrix can be inverted (non-singular)
! If singular matrix, we can't proceed so exit with
! an indication to Blossom of what went awry.
i_SSLK = i_N
i_SSLN = i_N
d_SSLEPSZ = d_ZERO ! use a standard pivot=0 detection
ALLOCATE(                         &
      da_SSLA(1:i_SSLN,1:i_SSLN), &
      da_SSLVW(1:i_SSLN),         &
      ia_SSLIP(1:i_SSLN),         &
      STAT=iEr)
IF (iEr /= i_OK) THEN   ! could not allocate array space
!   ________________________________________________________________
!  |                                                                |
!  | "Memory allocation error: da_SSLA,da_SSLVW,ia_SSLIP in INVERT" |
!  |________________________________________________________________|
!
   i_CCode = iEr
   iEr = i_INVRTMOD_ER002
   IF (ALLOCATED(da_SSLA))  DEALLOCATE(da_SSLA,  STAT=ios)
   IF (ALLOCATED(da_SSLVW)) DEALLOCATE(da_SSLVW, STAT=ios)
   IF (ALLOCATED(ia_SSLIP)) DEALLOCATE(ia_SSLIP, STAT=ios)
   DEALLOCATE(ia_KP, ia_KR, ia_KC, STAT=ios)
   RETURN   ! ... error exit ...
END IF
da_SSLA = da_W
!CALL DALU(da_SSLA,   &
!          i_SSLK,    &
!          i_SSLN,    &
!          d_SSLEPSZ, &
!          ia_SSLIP,  &
!          i_SSLIS,   &
!          da_SSLVW,  &
!          i_SSLICON)
!= DALU from Fijitsu Scientific Subroutine Library II (SSL II)     =!
!= An nXn non-singular real matrix is LU-decomposed using Crout's  =!
!= method.  DALUE returns an error code in i_SSLICON if the matrix =!
!= is singular. We use DALU only to test for singularity of the    =!
!= matrix, and use Mielke's matrix inversion code for non-singular =!
!= real matrices.                                                  =!
IF (i_SSLICON == i_MAT_SINGULARITY) THEN ! indicates near-zero pivot element
   iEr = i_MAT_SINGULARITY              ! or singular matrix
   i_CCode = i_MAT_SINGULARITY
   DEALLOCATE(da_SSLA, da_SSLVW, ia_SSLIP, ia_KP, ia_KR, ia_KC, STAT=ios)
   RETURN   ! ... error exit ...
END IF
DEALLOCATE(da_SSLA, da_SSLVW, ia_SSLIP, STAT=ios)
! Now get on with inverting the matrix
ia_KP = i_ZERO ! initialize array
Mainloop : DO i=1,i_N
   d_T = d_ZERO
   DO j=1,i_N
      IF (ia_KP(j) /= 1) THEN
         DO k=1,i_N
            IF (ia_KP(k)-1 > i_ZERO) THEN
               iEr = i_OK
               DEALLOCATE(ia_KP, ia_KR, ia_KC, STAT=ios)
               RETURN   ! ... normal exit ...
            END IF
            IF (ia_KP(k)-1 < i_ZERO) THEN
               IF (d_T < ABS(da_W(j,k))) THEN
                  i_IR = j
                  i_IC = k
                  d_T = ABS(da_W(j,k))
               END IF
            END IF
            call rchkusr()
         END DO
      END IF
   END DO
   ia_KP(i_IC) = ia_KP(i_IC) + 1
   IF (i_IR /= i_IC) THEN
      DO l=1,i_N
         d_T = da_W(i_IR,l)
         da_W(i_IR,l) = da_W(i_IC,l)
         da_W(i_IC,l) = d_T
         call rchkusr()
      END DO
   END IF
   ia_KR(i) = i_IR
   ia_KC(i) = i_IC
   d_T = da_W(i_IC,i_IC)
   da_W(i_IC,i_IC) = d_ONE
   da_W(i_IC,1:i_N) = da_W(i_IC,1:i_N)/d_T
   DO m=1,i_N
      IF (m /= i_IC) THEN
         d_T = da_W(m,i_IC)
         da_W(m,i_IC) = d_ZERO
         da_W(m,1:i_N) = da_W(m,1:i_N) - da_W(i_IC,1:i_N)*d_T
      END IF
      call rchkusr()
   END DO
END DO MainLoop
DO i=1,i_N
   j = i_N - i + 1
   IF (ia_KR(j) /= ia_KC(j)) THEN
      k = ia_KR(j)
      l = ia_KC(j)
      DO m=1,i_N
         d_T = da_W(m,k)
         da_W(m,k) = da_W(m,l)
         da_W(m,l) = d_T
         call rchkusr()
      END DO
   END IF
END DO
iEr = i_OK
! another normal exit
DEALLOCATE(ia_KP, ia_KR, ia_KC, STAT=ios)
RETURN   ! ... normal exit ...
END SUBROUTINE invert

!=============================================================================!

END MODULE invertmod


!     ******************************************************************
!     mrppmod.f90
!     Author: Jon D. Richards    U.S. Geological Survey
!     Last change: JDR 9/21/2007 11:20:05 AM
!     ******************************************************************
MODULE mrauxmod
!d USE debugmod                                                        ! debug!
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     MODULE mrauxmod
! DESCRIPTION:
!     Procedures used by various multi-response procedures.
! LANGUAGE:
!     Fortran 90/95
! AUTHOR:
!     Jon D. Richards, Operations Research Analyst       jon_richards@usgs.gov
!     Fort Collins Science Center                     http://www.fort.usgs.gov
!     U.S. Geological Survey          -          Biological Resources Division
! MODIFICATION HISTORY:
!     written - JDR  31 Aug 2000
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
IMPLICIT NONE
SAVE
CONTAINS
!     ALIGNBLKDATA    <File: "mrauxmod.f90: alignblkdata">
!     SCRNMISV        <File: "mrauxmod.f90: scrnmisv">
!     GETGPSIZNUMVALS <File: "mrauxmod.f90: getgpsiznumvals">
!     GETVARNAME      <File: "mrauxmod.f90: getvarname">
!     GETCMDGPVALS    <File: "mrauxmod.f90: getcmdgpvals">
!     GETACTIVEVARLST <File: "mrauxmod.f90: getactivevarlst">
!     SETGPVALTAGS    <File: "mrauxmod.f90: setgpvaltags">
!     GETCMDVARORDER  <File: "mrauxmod.f90: getcmdvarorder">
!     QSORTNDX        <File: "mrauxmod.f90: qsortndx">
!     GETNDX          <File: "mrauxmod.f90: getndx">
!     GETNUMMRVARS    <File: "mrauxmod.f90: getnummrvars">
!     GETMRVARLIST    <File: "mrauxmod.f90: getmrvarlist">

!=============================================================================!

SUBROUTINE alignblkdata(da_BlockV,    &
                        i_NumBlocks,  &
                        i_NumMRVars,  &
                        i_NumGroups,  &
                        l_IsAligned,  &
                        da_Data,      &
                        da_AlignVals, &
                        iEr, 		&
                        ch_MyMsg)
!d USE debugmod, ONLY: MyDebug                                         ! debug!
!___Imported Parameters and Variables:
USE ablparms, ONLY: i_SIZE_OF_REAL8
        ! ^-- Some high-level Blossom parameters.
! USE bfilemod, ONLY: ch_TEMP_FILE, i_BTempRecLen, i_TempRecLen, i_TEMP_UNIT

USE blcmnmod, ONLY: ch_NL, d_TWO, d_ZERO, i_NOT_OK,          &
      i_OK, i_OUT_OF_RANGE, l_TRUE
        ! ^-- Some global Blossom variables and parameters.
USE jonsmodule, ONLY: ch_EM00033, ch_EM00047, ch_EM00048, ch_EM00049
        ! ^-- Message handling, etc.

USE ioformod, ONLY: ch12_OutBuff, ch12_OutBuff2, ch12_OutBuff3, ch12_OutBuff4
        ! ^-- Output buffer variables.
!___Imported Procedures:

USE jonsmodule, ONLY: errhand, lf_Equals
        ! ^-- lf_Equals: Generic test for equality of two objects.
        ! ^-- ndperrhand: Give error message for floating point error.
IMPLICIT NONE
!___Dummy Arguments:
REAL (KIND(0D0)), INTENT(IN)    :: da_BlockV(:)  ! dim(i_NumCases)
INTEGER,          INTENT(IN)    :: i_NumBlocks
INTEGER,          INTENT(IN)    :: i_NumMRVars
INTEGER,          INTENT(IN)    :: i_NumGroups
LOGICAL, 		  INTENT(IN)    :: l_IsAligned
REAL (KIND(0D0)), INTENT(INOUT) :: da_Data(:,:,:)
REAL (KIND(0D0)), INTENT(OUT)   :: da_AlignVals(:,:)
                 ! dim(i_NumBlocks,i_NumMRVars)--^
INTEGER,          INTENT(OUT)   :: iEr
INTEGER, PARAMETER :: i_LEN_MY_EMSG=254
CHARACTER (LEN=i_LEN_MY_EMSG), INTENT(OUT) :: ch_MyMsg

! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     SUBROUTINE alignblkdata
! DESCRIPTION:
!     Obtain the active data from the Blossom TEMP data set.
!     If Needed then Align the data.
!        For each variable:
!        .  For each block:
!        .  .  Find block median;
!        .  .  For each value in block:
!        .  .  .  Align value around new block median of zero;
!        .  Find variable range (max-min);
!        .  For each value in variable:
!        .  .  Commensurate (divide by range);
! LANGUAGE:
!     Fortran 90, with Lahey Computer Systems LF90 intrinsics extensions
! AUTHOR:
!     Jon D. Richards, Operations Research Analyst       jon_richards@usgs.gov
!     Fort Collins Science Center                     http://www.fort.usgs.gov
!     U.S. Geological Survey          -          Biological Resources Division
! INVOKED BY:
!     runmrbp       <File: "domrpp.f90: runmrbp">         module
! INVOKES:
!     errhand       <File: "jonsmodule.f90: errhand">     module
!     lf_Equals     <File: "jonsmodule.f90: lf_Equals">   module
!     ndperrhand    <File: "jonsmodule.f90: ndperrhand">  module
! FILES:
!     ch_TempPathFile  <TEMP file>  OPEN (DIRECT) READ CLOSE
! DEVICES:
!     -

! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!___Procedure Parameters:
REAL (KIND(0D0)),  PARAMETER :: d_BLOCK_VAL_TOL = 0.00001D0
REAL (KIND(0D0)),  PARAMETER :: d_ONE_PLUS_ABIT = 1.0000000001D0
CHARACTER (LEN=*), PARAMETER ::    ch_PROC_NAME = "ALIGNBLKDATA"
!___Local Variables:
REAL (KIND(0D0)), ALLOCATABLE :: da_X(:,:,:)
REAL (KIND(0D0)), ALLOCATABLE :: da_BValues(:)
REAL (KIND(0D0)), ALLOCATABLE :: da_Values(:)
REAL (KIND(0D0)) :: d_A1, d_A2, d_DM1, d_DM2, d_Sum, d_Sum1
INTEGER :: i, j, k, l
INTEGER :: I1, I2
INTEGER :: i_Count
INTEGER :: i_KRec
INTEGER :: ios
!___Intrinsic Procedures:
INTRINSIC :: ABS, ALLOCATED, HUGE
                  !  ^-- Non-standard Lahey intrinsic.
                                                               ! debug!
iEr = i_OK ! no problem yet
! Open TEMP file to read from.

! For each active MRPP variable:
ALLOCATE(                        &
      da_BValues(1:i_NumGroups), &
       da_Values(1:i_NumMRVars),   &
      STAT=ios)
IF (ios /= i_OK) THEN
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |_____________________________
!  |                                                                |
!  | 'Memory allocation error: da_BValues,da_Values in ch_PROC_NAME |
!  |________________________________________________________________|
!
   CALL errhand(ch_MyMsg, i_Code=ios,        &
                ch_Msg=ch_EM00033, &
                ch_S1="da_BValues,da_Values", &
                ch_S2=ch_PROC_NAME)
   IF (ALLOCATED(da_BValues)) DEALLOCATE(da_BValues, STAT=ios)
   IF (ALLOCATED(da_Values)) DEALLOCATE(da_Values, STAT=ios)
   iEr = i_NOT_OK
   RETURN   ! ... error exit ...
END IF


IF (l_IsAligned) THEN
   ALLOCATE(da_X(1:i_NumGroups,1:i_NumBlocks,1:i_NumMRVars), STAT=ios)
   IF (ios /= i_OK) THEN
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |_______________
!  |                                                  |
!  | 'Memory allocation error: da_X in ' ch_PROC_NAME |
!  |__________________________________________________|
!
     CALL errhand(ch_myMsg,i_Code=ios,        &
                   ch_Msg=ch_EM00033, &
                   ch_S1="da_X",      &
                   ch_S2=ch_PROC_NAME)
      iEr = i_NOT_OK
      RETURN   ! ... error exit ...
   END IF
   da_X = da_Data ! Set array da_X to values of array da_Data
   ! Mielke's median calculation and block alignment loops:
   DO j=1,i_NumBlocks
      DO k=1, i_NumMRVars
         d_A1   = HUGE(1.0D200) ! I put in the HUGE function-JDR  31 Mar 2000
         d_Sum1 = d_A1
         DO I1=1,i_NumGroups
            d_Sum = d_ZERO
            DO I2=1,i_NumGroups
               d_Sum = d_Sum + ABS(da_Data(I2,j,k)-da_X(I1,j,k))
            call rchkusr()
            END DO

            IF (d_Sum < d_A1) THEN
               d_DM1 = da_X(I1,j,k)
               d_A1  = d_Sum
               d_A2  = d_A1*d_ONE_PLUS_ABIT
            END IF
            call rchkusr()

            IF (d_Sum < d_A2) THEN
               IF (.NOT.lf_Equals(d_DM1, da_X(I1,j,k))) THEN
                        !* <File: "jonsmodule.f90: lf_Equals">
                  d_DM2  = da_X(I1,j,k)
                  d_Sum1 = d_Sum
               END IF
            END IF
         END DO
         IF (d_Sum1 > d_A2) THEN
            d_DM2 = d_DM1
         END IF
         da_AlignVals(j,k) = (d_DM1+d_DM2)/d_TWO    ! median calculation
         call rchkusr()

      END DO
   END DO
   DO i=1,i_NumGroups
      DO j=1,i_NumBlocks
         DO k=1,i_NumMRVars
            da_Data(i,j,k) = da_Data(i,j,k) - da_AlignVals(j,k) ! alignment
            call rchkusr()
         END DO
      END DO
   END DO
END IF

IF (ALLOCATED(da_X)) DEALLOCATE(da_X, STAT=ios)

                                                               ! debug!
RETURN   ! ... normal exit ...
END SUBROUTINE alignblkdata
END MODULE mrauxmod



MODULE mrppmod

! MODIFICATION HISTORY:
!     Module created - JDR  21 Oct 1999
!     Mod - added (monte carlo) resampling from Mielke - JDR   2 May 2000
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
IMPLICIT NONE
SAVE
CONTAINS
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     MRPP           <File: "mrppmod.f90: mrpp">
!        MRPPDIST    <File: "mrppmod.f90: mrppdist">
!        SAMPWOR     <File: "mrppmod.f90: sampwor">
!        UNI         <File: "mrppmod.f90: uni">
!        MRPPCALC    <File: "mrppmod.f90: mrppcalc">
!     EMRPP          <File: "mrppmod.f90: emrpp">
!        EMRPPPERM   <File: "mrppmod.f90: emrppperm">
!        EMRPPDVAL   <File: "mrppmod.f90: emrppdval">
!     PTMP           <File: "mrppmod.f90: ptmp">
!        PTMPCOMB    <File: "mrppmod.f90: ptmpcomb">
!        PTMPXACT    <File: "mrppmod.f90: ptmpxact">
!     MRBP           <File: "mrppmod.f90: mrbp">
!        MRBPCALC    <File: "mrppmod.f90: mrbpcalc">
!     EMRBP          <File: "mrppmod.f90: emrbp">
!        EMRBPRANK   <File: "mrppmod.f90: emrbprank">
!        FACT2       <File: "mrppmod.f90: fact2">
!        FACT3       <File: "mrppmod.f90: fact3">
!        FACT4       <File: "mrppmod.f90: fact4">
!        FACT5       <File: "mrppmod.f90: fact5">
!        FACT6       <File: "mrppmod.f90: fact6">
!        FACT7       <File: "mrppmod.f90: fact7">
!        FACT8       <File: "mrppmod.f90: fact8">
!        FACT9       <File: "mrppmod.f90: fact9">
!        EMRBPDVAL   <File: "mrppmod.f90: emrbpdval">
!     EMVPTMP        <File: "mrppmod.f90: emvptmp">
!        EMVPTMPCOMB <File: "mrppmod.f90: emvptmpCOMB">
!        EMVPTMPXACT <File: "mrppmod.f90: emvptmpXACT">

!=============================================================================!

SUBROUTINE mrpp(d_NumObs, i_NumVars, d_V, d_Sup, i_CForm, i_LZ, d_NumGrps,    &
      d_NInterv, ia_GrpSizes, i_NumPerms, l_DoResample, da_Datas, da_XI1,     &
      d_T, d_DBar, d_D1, d_Var, d_Gam, d_PValue, da_YHot, iEr,i_Seed,l_SaveTest,da_STV)
!d USE debugmod, ONLY: MyDebug                                         ! debug!
!___Imported Parameters and Variables:
USE blcmnmod, ONLY: d_HALF, d_ONE, d_TWO, d_ZERO, i_OK
        ! ^-- Some global Blossom variables and parameters.
USE jonsmodule, ONLY: i_CCode, i_MRPPMOD_ER001, i_MRPPMOD_ER002,              &
      i_MRPPMOD_ER028, i_MRPPMOD_ER029
        ! ^-- Message handling, etc.
USE mrolamod, ONLY: i_MAT_SINGULARITY
        ! ^-- Some high-level Blossom parameters.
!___Imported Procedures:
USE jonsmodule, ONLY: lf_Equals
        ! ^-- lf_Equals: Generic test for equality of two objects.
USE pv_modul, ONLY: pvalue
        ! ^-- pvalue: compute P-value
IMPLICIT NONE
!things from jonsmodule which won't compile
!INTEGER :: i_CCode
!INTEGER, PARAMETER :: i_MRPPMOD_ER001 = 40001
!INTEGER, PARAMETER :: i_MRPPMOD_ER002 = 40002
!INTEGER, PARAMETER :: i_MRPPMOD_ER028 = 40028
!INTEGER, PARAMETER :: i_MRPPMOD_ER029 = 40029

!___Dummy Arguments:
REAL (KIND(0D0)), INTENT(IN)    :: d_NumObs  ! number of observations
INTEGER,          INTENT(IN)    :: i_NumVars ! number of responses
REAL (KIND(0D0)), INTENT(IN)    :: d_V       ! distance exponent
REAL (KIND(0D0)), INTENT(IN)    :: d_Sup     ! distance upper bound
INTEGER,          INTENT(IN)    :: i_CForm   ! weighting form
INTEGER,          INTENT(IN)    :: i_LZ      ! Hotelling's Commn 1=Y,0=N
REAL (KIND(0D0)), INTENT(IN)    :: d_NumGrps ! number of groups
REAL (KIND(0D0)), INTENT(IN)    :: d_NInterv ! numb intervals/cycle
INTEGER,          INTENT(IN)    :: ia_GrpSizes(:) ! group sizes
INTEGER,          INTENT(IN)    :: i_NumPerms ! num resamples to do
LOGICAL,          INTENT(IN)    :: l_DoResample ! do resample?
REAL (KIND(0D0)), INTENT(INOUT) :: da_Datas(:,:) ! observation data
REAL (KIND(0D0)), INTENT(OUT)   :: da_XI1(:)   ! average group dist.
REAL (KIND(0D0)), INTENT(OUT)   :: d_T      ! Test Statistic
REAL (KIND(0D0)), INTENT(OUT)   :: d_DBar   ! Observed Delta
REAL (KIND(0D0)), INTENT(OUT)   :: d_D1     ! Expected Delta
REAL (KIND(0D0)), INTENT(OUT)   :: d_Var    ! Variance of Delta
REAL (KIND(0D0)), INTENT(OUT)   :: d_Gam    ! Skewness of Delta
REAL (KIND(0D0)), INTENT(OUT)   :: d_PValue ! P-value of <= Delta
REAL (KIND(0D0)), INTENT(OUT)   :: da_YHot(:,:)  ! Variance/cov matrix
INTEGER,          INTENT(OUT)   :: iEr      ! return error code
INTEGER,	  	  INTENT(INOUT) :: i_Seed
LOGICAL, 		  INTENT(IN) :: l_SaveTest
REAL (KIND(0D0)), INTENT(OUT) :: da_STV(:)
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     SUBROUTINE mrpp
! DESCRIPTION:
!     Multi-Response Permutation Procedures
!     This code was written by
!       Kenneth J. Berry and Paul W. Mielke, Jr.
!        http://www.stat.colostate.edu/~mielke/mielkesoftware.html
!       Department of Statistics
!       Colorado State University
!       Fort Collins, Colorado 80523

!  Dummy Argument Description:
!                              (IN) d_NumObs  number of observations
!                              (IN) i_NumVars number of response variables
!                              (IN) d_V       distance exponent
!                              (IN) d_Sup     distance upper bound
!                              (IN) i_CForm   weighting form
!                              (IN) i_LZ      Hotelling's Commens. (1=Y,0=N)
!                              (IN) d_NumGrps number of groups
!                              (IN) d_NInterv number of intervals in arc circle
!              dim(i_NumGps)    (IN) ia_GrpSizes group sizes
!                              (IN) i_NumPerms number (monte carlo) resamples
!                              (IN) l_DoResample Do (MC) resampling
!   dim(i_NumObs,i_NumVars+1) (INOUT) da_Datas  observation data
!              dim(i_NumGps)   (OUT) da_XI1    average group distance
!                             (OUT) d_T       Test Statistic
!                             (OUT) d_DBar    Observed Delta
!                             (OUT) d_D1      Expected Delta
!                             (OUT) d_Var     Variance of Delta
!                             (OUT) d_Gam     Skewness of Delta
!                             (OUT) d_PValue  P-value of smaller or equal Delta
!    dim(i_NumVars,i_NumVars)   (OUT) da_YHot   Variance/covariance matrix
!                             (OUT) iEr      return error code

! INVOKED BY:
!     runmrpp   <File: "domrpp.f90: runmrpp">       module
! INVOKES:
!     lf_Equals <File: "jonsmodule.f90: lf_Equals"> module
!     mrppcalc  <File: "mrppmod.f90: mrppcalc">     module
!     mrppdist  <File: "mrppmod.f90: mrppdist">     module
!     pvalue    <File: "pv_modul.f90: pvalue">      module
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!___Program Parameters:

INTEGER, PARAMETER :: i_DO_HOT = 1  ! Do Hotelling's commensuration.
!___Local Variables:
REAL (KIND(0D0)), ALLOCATABLE :: da_D(:,:)
REAL (KIND(0D0)), ALLOCATABLE :: da_DPacked(:)
REAL (KIND(0D0)), ALLOCATABLE :: da_YHotHold(:,:)
REAL (KIND(0D0)), ALLOCATABLE :: da_C(:)
REAL (KIND(0D0)), ALLOCATABLE :: da_DSub1(:)
REAL (KIND(0D0)), ALLOCATABLE :: da_DSub2(:)
!x REAL (KIND(0D0)) :: da_U(17)
!x REAL (KIND(0D0)) :: d_Srs, d_Trs
REAL (KIND(0D0)) :: d_D3O, d_DLit1, d_DLit2, d_DLit3
REAL (KIND(0D0)) :: d_P1, d_P2, d_P3, d_P4
REAL (KIND(0D0)) :: d_Prob
! REAL :: start_time, end_time
INTEGER :: ios
INTEGER :: i_NumVarsP1, i_LZUse, i_NumObs, i_NumGrps
!___Intrinsic Procedures:
INTRINSIC :: INT
call rchkusr()
                                                       ! debug!
iEr = i_OK ! no problem yet
d_PValue = d_ZERO
i_LZUse  = i_LZ

i_NumGrps   = INT(d_NumGrps) ! Number of Groups

i_NumObs    = INT(d_NumObs)  ! Number of Observations
i_NumVarsP1 = i_NumVars + 1  ! Number of Response Variables + 1

!###############################################################

!===============================================================


IF (ALLOCATED(da_YHotHold)) DEALLOCATE(da_YHotHold)
IF (i_LZ == i_DO_HOT) THEN  ! We need to hold copy of un-inverted var/cov matrix.
   ALLOCATE(da_YHotHold(1:i_NumVars,1:i_NumVars), STAT=iEr)

IF (iEr /= i_OK) THEN
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |_____________
!  |                                                |
!  | 'Memory allocation error: da_YHotHold in MRPP' |
!  |________________________________________________|
!
      i_CCode = iEr
      iEr = i_MRPPMOD_ER001

      GOTO 90000 ! RETURN   ! ... error exit ...
   ELSE
      da_YHotHold = d_ZERO
   END IF
ELSE
   ALLOCATE(da_YHotHold(1:1,1:1), STAT=iEr)


   IF (iEr /= i_OK) THEN
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |_____________
!  |                                                |
!  | 'Memory allocation error: da_YHotHold in MRPP' |
!  |________________________________________________|
!
      i_CCode = iEr
      iEr = i_MRPPMOD_ER001

      GOTO 90000 ! RETURN   ! ... error exit ...
   ELSE
      da_YHotHold = d_ZERO
   END IF
END IF
                                                           ! debug!
!///////////////////////////////////////////////////////////////////////////
! SKIP ALLOCATION OF da_D altogether ?
! SOME CRITICAL CUTOVER(s) TO CORRECT DIST PROCEDURE
! *******
IF (i_NumObs < 0) THEN  ! essentially skip this legacy version altogether.

   ALLOCATE(da_D(1:i_NumObs,1:i_NumObs), &
            da_DPacked(1:1), &
            STAT=iEr)
   IF (iEr /= i_OK) THEN
   !   __________________________________
   !  |                                  |
   !  | IOSTAT_MSG(i_Code) error message |______
   !  |                                         |
   !  | 'Memory allocation error: da_D in MRPP' |
   !  |_________________________________________|
   !
      i_CCode = iEr
      iEr = i_MRPPMOD_ER028

      GOTO 90000 ! RETURN   ! ... error exit ...
   END IF
                                                        ! debug!
ELSE

   ALLOCATE(da_DPacked(1:i_NumObs+i_NumObs*(i_NumObs-1)/2), &
            da_D(1:1,1:1), &
            STAT=iEr)
   IF (iEr /= i_OK) THEN
   !   __________________________________
   !  |                                  |
   !  | IOSTAT_MSG(i_Code) error message |____________
   !  |                                               |
   !  | 'Memory allocation error: da_DPacked in MRPP' |
   !  |_______________________________________________|
   !
      i_CCode = iEr
      iEr = i_MRPPMOD_ER029
                                                       ! debug!
      GOTO 90000 ! RETURN   ! ... error exit ...
   END IF
END IF
!///////////////////////////////////////////////////////////////////////////
                                                           ! debug!
ALLOCATE(                           &
       da_C(1:i_NumGrps),           &
   da_DSub1(1:i_NumObs),            & ! compiler says this not used
   da_DSub2(1:i_NumObs),            & ! compiler says this not used
   STAT=iEr)
IF (iEr /= i_OK) THEN
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |________________________
!  |                                                           |
!  | 'Memory allocation error: da_C,da_DSub1,da_DSub2 in MRPP' |
!  |___________________________________________________________|
!
   i_CCode = iEr
   iEr = i_MRPPMOD_ER002
                                                          ! debug!
   GOTO 90000 ! RETURN   ! ... error exit ...
ELSE
       da_C = d_ZERO
   da_DSub1 = d_ZERO
   da_DSub2 = d_ZERO
END IF

call rchkusr()
   CALL mrppdist_1d(d_NumObs, i_NumVars, d_V, d_Sup, i_CForm, d_NumGrps,      &
         i_NumGrps, i_NumObs, i_NumVarsP1, d_NInterv, i_LZUse, ia_GrpSizes,   &
         d_D3O, d_DLit1, d_DLit2, d_DLit3, d_P1, d_P2, d_P3, d_P4, d_DBar,    &
         da_C, da_XI1, da_Datas, da_DPacked, da_DSub1, da_DSub2, da_YHot,     &
         da_YHotHold, i_NumPerms, l_DoResample, d_Prob, iEr,i_Seed,l_SaveTest,da_STV)


IF (iEr /= i_OK) THEN

   DEALLOCATE(da_YHotHold, da_C, da_D, da_DPacked, da_DSub1, da_DSub2, &
         STAT=ios)
   GOTO 90000 ! RETURN   ! ... error exit ...
END IF

IF (ALLOCATED(da_DPacked)) DEALLOCATE(da_DPacked)
IF (ALLOCATED(da_D))       DEALLOCATE(da_D)

IF (l_DoResample) THEN
   d_PValue = d_Prob
ELSE
   CALL mrppcalc(i_NumObs, i_NumGrps, d_NumObs, ia_GrpSizes, d_D3O, d_DLit1,  &
         d_DLit2, d_DLit3, d_P1, d_P2, d_P3, d_P4, d_DBar, da_C, d_D1, d_Var, &
         d_T, d_Gam) !* <File: "mrppmod.f90: mrppcalc">
           ! If variance is d_ZERO, d_T and d_Gam are undefined, so can't
           ! get PROB, (which we will say in this case is 1.0)

   IF (lf_Equals(d_Var, d_ZERO)) THEN
       !* <File: "jonsmodule.f90: lf_Equals">
      d_PValue = d_ONE ! No variance, so P-value set to 1.0
   ELSE
      d_PValue = pvalue(d_T, d_Gam)   ! Routine from module pv_modul
                 !* <File: "pv_modul.f90: pvalue">
   END IF
END IF

IF (ALLOCATED(da_C)) DEALLOCATE(da_C, STAT=ios)

IF (ALLOCATED(da_DSub1)) DEALLOCATE(da_DSub1, STAT=ios)

IF (ALLOCATED(da_DSub2)) DEALLOCATE(da_DSub2, STAT=ios)

IF (i_LZ == i_DO_HOT) THEN  ! If Hotelling's commensuration was done.

   da_YHot = da_YHotHold ! Give the caller back the variance/covariance matrix.

END IF

DEALLOCATE(da_YHotHold, STAT=ios)

90000 CONTINUE

RETURN   ! ... normal exit ...
END SUBROUTINE mrpp

!=============================================================================!

SUBROUTINE mrppdist_1d(d_NumObs, i_NumVars, d_V, d_Sup, i_CForm, d_NumGrps,   &
      i_NumGrps, i_NumObs, i_NumVarsP1, d_NInterv, i_LZ, ia_GrpSizes, d_D3O,  &
      d_DLit1, d_DLit2, d_DLit3, d_P1, d_P2, d_P3, d_P4, d_DBar, da_C,        &
      da_XI1, da_Datas, da_D, da_DSub1, da_DSub2, da_YHot, da_YHotHold,       &
      i_NumPerms, l_DoResample, d_Prob, iEr,i_Seed,l_SaveTest,da_STV)

USE blcmnmod, ONLY: d_ONE, d_TWO, d_ZERO, i_DOS_ERR_EXIT, i_OK, i_ONE, i_ZERO
        ! ^-- Some global Blossom variables and parameters.
USE ioformod, ONLY: ch25_OutBuff, ch12_OutBuff
        ! ^-- Buffers for character output of numbers.
USE jonsmodule, ONLY: i_CCode, i_MRPPMOD_ER003, i_MRPPMOD_ER026,              &
      i_MRPPMOD_ER027
        ! ^-- Message handling, etc.
USE mrolamod, ONLY: i_C_FORM_1, i_C_FORM_2, i_C_FORM_3, i_C_FORM_4,           &
      i_DO_HOTELLING, i_MAT_SINGULARITY, i_RandNumSeed
        ! ^-- Some high-level Blossom parameters.
!___Imported Procedures:
USE invertmod, ONLY: invert
        ! ^-- invert: matrix inversion routine.
USE jonsmodule, ONLY: lf_Equals
        ! ^-- lf_Equals: Generic test for equality of two objects.
USE mt19937, ONLY: init_genrand
        ! ^-- Mersenne Twister Pseudo Random Number Generator
IMPLICIT NONE

INTEGER, PARAMETER :: i_SAVETEST_UNIT = 89 ! File Unit
CHARACTER (LEN=260) :: ch_SaveTestFile ! current Blossom SAVETEST file.
CHARACTER (LEN=260) :: ch_UseFile  ! current Blossom USE file.CHARACTER (LEN=260) :: ch_UseFile  ! current Blossom USE file.
CHARACTER (LEN=260) :: ch_OutPath     = " "
CHARACTER (LEN=260) :: ch_UsePath     = " " ! added August 2007 - jdr
!___Dummy Arguments:
REAL (KIND(0D0)), INTENT(IN)    :: d_NumObs  ! Number of Obs
INTEGER,          INTENT(IN)    :: i_NumVars ! Number of Responses
REAL (KIND(0D0)), INTENT(IN)    :: d_V       ! Distance exponent
REAL (KIND(0D0)), INTENT(IN)    :: d_Sup     ! Cut-off value
INTEGER,          INTENT(IN)    :: i_CForm   ! C (weight)
REAL (KIND(0D0)), INTENT(IN)    :: d_NumGrps ! Number of Groups
INTEGER,          INTENT(IN)    :: i_NumGrps
INTEGER,          INTENT(IN)    :: i_NumObs
INTEGER,          INTENT(IN)    :: i_NumVarsP1
REAL (KIND(0D0)), INTENT(IN)    :: d_NInterv
INTEGER,          INTENT(INOUT) :: i_LZ
INTEGER,          INTENT(IN)    :: ia_GrpSizes(:) ! dim(i_NumCases) only use (i_NumGrps)
REAL (KIND(0D0)), INTENT(OUT)   :: d_D3O
REAL (KIND(0D0)), INTENT(OUT)   :: d_DLit1
REAL (KIND(0D0)), INTENT(OUT)   :: d_DLit2
REAL (KIND(0D0)), INTENT(OUT)   :: d_DLit3
REAL (KIND(0D0)), INTENT(OUT)   :: d_P1
REAL (KIND(0D0)), INTENT(OUT)   :: d_P2
REAL (KIND(0D0)), INTENT(OUT)   :: d_P3
REAL (KIND(0D0)), INTENT(OUT)   :: d_P4
REAL (KIND(0D0)), INTENT(OUT)   :: d_DBar
REAL (KIND(0D0)), INTENT(OUT)   :: da_C(:)     ! dim(i_NumGrps)
REAL (KIND(0D0)), INTENT(OUT)   :: da_XI1(:)   ! dim(i_NumGrps)
REAL (KIND(0D0)), INTENT(INOUT) :: da_Datas(:,:)
          !       dim(i_NumObs,i_NumVarsP1)--^
REAL (KIND(0D0)), INTENT(OUT)   :: da_D(:) ! dim(i_NumObs+i_NumObs*i_NumObs-i_NumObs/2)
REAL (KIND(0D0)), INTENT(OUT)   :: da_DSub1(:) ! dim(i_NumObs)
REAL (KIND(0D0)), INTENT(OUT)   :: da_DSub2(:) ! dim(i_NumObs)
REAL (KIND(0D0)), INTENT(OUT)   :: da_YHot(:,:)
REAL (KIND(0D0)), INTENT(OUT)   :: da_YHotHold(:,:)
INTEGER,          INTENT(IN)    :: i_NumPerms
LOGICAL,          INTENT(IN)    :: l_DoResample
REAL (KIND(0D0)), INTENT(OUT)   :: d_Prob
INTEGER,          INTENT(OUT)   :: iEr
INTEGER,	  	  INTENT(INOUT) :: i_Seed
LOGICAL, 		  INTENT(IN) :: l_SaveTest
REAL (KIND(0D0)), INTENT(OUT) :: da_STV(:)

! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     SUBROUTINE mrppdist
! DESCRIPTION:
!     calculates the distances between the data points, plus the parameters
!     which are directly dependent on the distances.
! LANGUAGE:
!     Fortran 90/95
! AUTHOR:
!     Dr. Paul W. Mielke, Jr., Dept of Statistics, Colorado State University
!        http://www.stat.colostate.edu/~mielke/mielkesoftware.html
!     Modifications:
!        Jon D. Richards, Operations Research Analyst    jon_richards@usgs.gov
!        Fort Collins Science Center                  http://www.fort.usgs.gov
!        U.S. Geological Survey  -  Biological Resources Division
! INVOKED BY:
!     mrpp      <File: "mrppmod.f90: mrpp">         module
! INVOKES:
!     invert    <File: "invrtmod.f90: invert">      module
!     lf_Equals <File: "jonsmodule.f90: lf_Equals"> module
!     sampwor   <File: "mrppmod.f90: sampwor">      module
! FILES:
!     -
!___Procedure Parameters:
REAL (KIND(0D0)), PARAMETER :: d_PREC = 1.0D-12
!___Local Variables:
REAL (KIND(0D0)), ALLOCATABLE :: da_UHot(:)
REAL (KIND(0D0)), ALLOCATABLE :: da_XI(:)   ! dim(1:i_NumGrps)
REAL (KIND(0D0)), ALLOCATABLE :: da_DataHold(:,:)
REAL (KIND(0D0)) :: d_FK, d_Y, d_DX, d_DZ
REAL (KIND(0D0)) :: d_Sum
INTEGER :: i, j, k, l
INTEGER :: ix, ip, jp
INTEGER :: ios
INTEGER :: IS, JS, m
INTEGER :: i_MA, i_MP, i_IW
LOGICAL :: l_NonArc, l_NonHot, l_D2Z
INTEGER :: ksiz
!___Intrinsic Procedures:
INTRINSIC :: ABS, ALLOCATED, DBLE, MIN, NINT, REAL, SUM
!___Statement Function:
INTEGER dispos
dispos(i, j) = i + (2*i_NumObs-j)*(j-1)/2

iEr = i_OK ! no problem yet
d_Prob = d_ZERO
l = 1
m = ia_GrpSizes(l)

IF (l_DoResample) THEN
   ALLOCATE(da_XI(1:i_NumGrps), STAT=ios)
   IF (ios /= i_OK) THEN
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |___________
!  |                                              |
!  | 'Memory allocation error: da_XI in MRPPDIST' |
!  |______________________________________________|
!
      i_CCode = ios
      iEr = i_MRPPMOD_ER026

      GOTO 90000 ! RETURN   ! ... error exit ....
   END IF
ELSE
   ALLOCATE(da_XI(1:1), STAT=ios)
   IF (ios /= i_OK) THEN
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |___________
!  |                                              |
!  | 'Memory allocation error: da_XI in MRPPDIST' |
!  |______________________________________________|
!
      i_CCode = ios
      iEr = i_MRPPMOD_ER026

      GOTO 90000 ! RETURN   ! ... error exit ....
   END IF
END IF
! If we want to resample, we need to use copy of data to permute
IF (l_DoResample) THEN
   ALLOCATE(da_DataHold(1:i_NumObs,1:i_NumVarsP1),STAT=iEr)
   IF (iEr /= i_OK) THEN
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |_____________
!  |                                                |
!  | 'Memory allocation error: da_DataHold in MRPP' |
!  |________________________________________________|
!
      i_CCode = iEr
      iEr = i_MRPPMOD_ER027

      GOTO 90000 ! RETURN   ! ... error exit ...
   END IF
   da_DataHold = da_Datas
END IF

call rchkusr()

d_DLit1  = d_ZERO
d_DLit2  = d_ZERO
d_DLit3  = d_ZERO
da_XI1   = d_ZERO  ! Initialize array to zero.
da_DSub1 = d_ZERO  ! Initialize array to zero.
da_DSub2 = d_ZERO  ! Initialize array to zero.
IF (i_LZ == i_DO_HOTELLING) THEN ! do Hotelling's (var/covar) adjustment
   ALLOCATE(da_UHot(1:i_NumVars), STAT=iEr)
   IF (iEr /= i_OK) THEN
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |_____________
!  |                                                |
!  | 'Memory allocation error: da_UHot in MRPPDIST' |
!  |________________________________________________|
!
      IF (ALLOCATED(da_XI)) DEALLOCATE(da_XI)
      i_CCode = iEr
      iEr = i_MRPPMOD_ER003

      GOTO 90000 ! RETURN   ! ... error exit ...
   END IF
   DO i=1,i_NumVars                     ! for each variable...
      d_Sum = SUM(da_Datas(1:i_NumObs,i))
      da_UHot(i) = d_Sum / d_NumObs !...get average for this variable.
      call rchkusr()
   END DO
   DO i=1,i_NumVars     ! For each variable
      DO j=1,i_NumVars  !   by each variable
      da_YHot(i,j) = SUM((da_Datas(1:i_NumObs,i)-da_UHot(i)) *                &
                         (da_Datas(1:i_NumObs,j)-da_UHot(j)))
                         call rchkusr()
      END DO
   END DO

   DEALLOCATE(da_UHot, STAT=ios)
   da_YHotHold = da_YHot ! Save an uninverted matrix for caller.
   CALL invert(i_NumVars, da_YHot, iEr) ! from module INVRTMOD
   IF (iEr /= i_OK) THEN

IF(ALLOCATED(da_XI)) DEALLOCATE(da_XI)
      GOTO 90000 ! RETURN   ! ... error exit ...
   END IF
ELSE       ! no adjustment done
   da_YHotHold = d_ZERO
   da_YHot = d_ZERO
END IF ! End Hotelling's var/covar matrix setup
call rchkusr()

l_NonArc = lf_Equals(d_NInterv, d_ZERO) !* <File: "jonsmodule.f90: lf_Equals">
da_D = d_ZERO  ! Initialize array to d_ZERO.
DO i=1,i_NumObs       ! Compute the distances

   DO j=1,i_NumObs    !   from observation to observation.

      IF (j <= i) THEN
         ip = i
         jp = j
      ELSE
         ip = j
         jp = i
      END IF
      ix = dispos(ip, jp) ! ip+(2*i_NumObs-jp)*(jp-1)/2
      IF (l_NonArc) THEN    ! for non ARC distance data:
         IF (j >= i) THEN
            DO k=1,i_NumVars
               IF (i_LZ /= i_DO_HOTELLING) THEN
                  ! da_D(i,j)=da_D(i,j)+(da_Datas(i,k)-da_Datas(j,k))**2
                  da_D(ix) = da_D(ix) + (da_Datas(i,k)-da_Datas(j,k))**2
               ELSE     ! Hotelling's adjustment:
                  DO l=1,i_NumVars

                     da_D(ix) = da_D(ix) + (da_Datas(i,k)-da_Datas(j,k)) *    &
                                                  da_YHot(k,l)*               &
                                           (da_Datas(i,l)-da_Datas(j,l))
                                           call rchkusr()
                  END DO
               END IF
            END DO

            da_D(ix) = da_D(ix)**(d_V/d_TWO)

         END IF
      ELSE          ! for ARC distance data: (univariate only)

         da_D(ix) = MIN(ABS(da_Datas(i,1)-da_Datas(j,1)),                     &
              d_NInterv-ABS(da_Datas(i,1)-da_Datas(j,1)))
         !.. IF (.NOT.lf_Equals(d_V,d_ONE)) da_D(I,J) = da_D(I,J)**d_V ! adjust dist.
         ! da_D(i,j) = da_D(i,j)**d_V ! adjust distance.
         da_D(ix) = da_D(ix)**d_V ! adjust distance.
      END IF
      ! This is where the truncation value is used. If we want we
      ! can comment the next line out of the code and the program
      ! will ignore any d_Sup value. For now, we will control it like
      ! this:
      IF (d_Sup > d_ZERO) then ! If sup negative or zero, ignore truncation.
         ! IF (da_D(i,j) > d_Sup) da_D(i,j) = d_Sup
         IF (da_D(ix) > d_Sup) da_D(ix) = d_Sup
      END IF

      d_DLit1 = d_DLit1 + da_D(ix)

      d_DLit2 = d_DLit2 + da_D(ix)**2
      d_DLit3 = d_DLit3 + da_D(ix)**3
      da_DSub1(i) = da_DSub1(i) + da_D(ix)
      da_DSub2(i) = da_DSub2(i) + da_D(ix)**2
      IS = NINT(da_Datas(i,i_NumVarsP1))
      JS = NINT(da_Datas(j,i_NumVarsP1))
      IF (IS <= i_NumGrps) THEN
         IF (IS == JS) THEN
            ! da_XI1(IS) = da_XI1(IS) + da_D(i,j)
            da_XI1(IS) = da_XI1(IS) + da_D(ix)
         END IF
      END IF
      call rchkusr()
   END DO
END DO


! Compute weighting constants for the I groups
da_C = d_ZERO

SELECT CASE (i_CForm)
   CASE (i_C_FORM_1)
      d_FK = d_ZERO
      DO i=1,i_NumGrps
         d_FK = d_FK + DBLE(ia_GrpSizes(i))
      END DO
      DO i=1,i_NumGrps
         da_C(i) = DBLE(ia_GrpSizes(i))/d_FK
      END DO
   CASE (i_C_FORM_2)
      d_FK = d_ZERO
      DO i=1,i_NumGrps
         d_FK = d_FK + DBLE(ia_GrpSizes(i))
      END DO
      DO i=1,i_NumGrps
         da_C(i) = DBLE(ia_GrpSizes(i)-1)/(d_FK-d_NumGrps)
      END DO
   CASE (i_C_FORM_3)
      DO i=1,i_NumGrps
         da_C(i) = d_ONE/d_NumGrps
      END DO
   CASE (i_C_FORM_4)
      d_Y = d_ZERO
      DO i=1,i_NumGrps
         d_Y = d_Y + DBLE(ia_GrpSizes(i)*(ia_GrpSizes(i)-1))
      END DO
      DO i=1,i_NumGrps
         da_C(i) = DBLE(ia_GrpSizes(i)*(ia_GrpSizes(i)-1))/d_Y
      END DO
END SELECT
call rchkusr()

l_NonHot = i_LZ /= 1  ! Don't do Hotelling comm. when i_LZ not equal to 1.
IF (l_DoResample) THEN

        ! BEGIN RESAMPLING estimate of P-Value:
   DO i=1,i_NumGrps
      da_XI1(i) = da_XI1(i)/DBLE(ia_GrpSizes(i)*(ia_GrpSizes(i)-1))
   END DO
   d_DBar = SUM(da_C(1:i_NumGrps)*da_XI1(1:i_NumGrps))
   d_DX = d_DBar*(d_ONE + d_PREC)
   IF (l_SAVETEST) THEN ! We want to save original and all resampled test statistics
      da_STV(1) = d_DX
   END IF
   !mielke i_MP = i_ZERO
   i_MP = i_ONE ! The first time through we count as a sample that
                ! statisfies the test criterion.
   !mielke DO i_IW=1,i_NumPerms
   DO i_IW=1,i_NumPerms-1 ! The first time through was one of our samples.
      call rchkusr()
      l_D2Z = .TRUE.
      CALL sampwor(i_NumObs, i_NumObs, i_NumVars, da_Datas, l_D2Z)

      l = 1
      i_MA = ia_GrpSizes(l)
      DO i=1,i_NumObs
         DO
            IF (i <= i_MA) EXIT
            l = l + 1
            IF (l <= i_NumGrps) THEN
               i_MA = i_MA + ia_GrpSizes(l)
            ELSE
               i_MA = i_NumObs
            END IF
            call rchkusr()
         END DO
         da_Datas(i,i_NumVars+1) = DBLE(l)
      END DO
      da_XI = d_ZERO ! Initialize array.
      IF (l_NonHot) THEN
         ! NO HOTELLING Commensuration:
         DO i=1,i_NumObs
            DO j=1,i_NumObs
               IF (j <= i) THEN
                  ip = i
                  jp = j
               ELSE
                  ip = j
                  jp = i
               END IF
               ix = dispos(ip, jp) ! ip+(2*i_NumObs-jp)*(jp-1)/2
               IF (l_NonArc) THEN   ! NO Arc distances:
                  d_Sum = SUM((da_Datas(i,1:i_NumVars)-da_Datas(j,1:i_NumVars))**2)
                  ! da_D(i,j) = d_Sum**(d_V/d_TWO)
                  da_D(ix) = d_Sum**(d_V/d_TWO)
               ELSE    ! ARC DISTANCE data: (univariate only)

                  da_D(ix) = MIN(ABS(da_Datas(i,1)-da_Datas(j,1)),            &
                       d_NInterv-ABS(da_Datas(i,1)-da_Datas(j,1)))

                  da_D(ix) = da_D(ix)**d_V ! adjust distance
               END IF
               IF (d_Sup > d_ZERO) then ! If sup neg or zero, ignore truncation.
                IF (j >= i) THEN

                  IF (da_D(ix) > d_Sup) da_D(ix) = d_Sup
                END IF
               END IF
               IS = NINT(da_Datas(i,i_NumVarsP1))
               JS = NINT(da_Datas(j,i_NumVarsP1))
               IF (IS <= i_NumGrps) THEN
                  IF (IS == JS) THEN
                     ! da_XI(IS) = da_XI(IS) + da_D(i,j)
                     da_XI(IS) = da_XI(IS) + da_D(ix)
                  END IF
               END IF
               call rchkusr()
            END DO
         END DO
        ! END NO HOTELLING Commensuration
      ELSE
      ! HOTELLING COMMENSURATION of variables:
      ! WE SHOULD use the inverted variance/covariance matrix from above,
      ! there is no need to re-compute each time. - JDR  16 May 2000

         da_D = d_ZERO ! Initialize array.
         DO i=1,i_NumObs
            DO j=1,i_NumObs
               IF (j <= i) THEN
                  ip = i
                  jp = j
               ELSE
                  ip = j
                  jp = i
               END IF
               ix = dispos(ip, jp)
               IF (l_NonArc) THEN
                  DO k=1,i_NumVars
                     DO l=1,i_NumVars

                        da_D(ix) = da_D(ix) +                                 &
                                       (da_Datas(i,k)-da_Datas(j,k))*         &
                                              da_YHot(k,l)*                   &
                                       (da_Datas(i,l)-da_Datas(j,l))
                                       call rchkusr()
                     END DO
                  END DO

                  da_D(ix) = da_D(ix)**(d_V/d_TWO)
               ELSE

                  da_D(ix) = MIN(ABS(da_Datas(i,1)-da_Datas(j,1)),            &
                       d_NInterv-ABS(da_Datas(i,1)-da_Datas(j,1)))

                  da_D(ix) = da_D(ix)**d_V ! adjust distance.
               END IF
               IF (d_Sup > d_ZERO) then ! If sup neg or zero, ignore truncation.

                  IF (da_D(ix) > d_Sup) da_D(ix) = d_Sup
               END IF
               IS = NINT(da_Datas(i,i_NumVarsP1))
               JS = NINT(da_Datas(j,i_NumVarsP1))
               IF (IS <= i_NumGrps) THEN
                  IF (IS == JS) THEN

                     da_XI(IS) = da_XI(IS) + da_D(ix)
                  END IF
               END IF
               call rchkusr()
            END DO
         END DO
        ! END HOTELLING Commensuration
      END IF

      DO i=1,i_NumGrps
         da_XI(i) = da_XI(i)/DBLE(ia_GrpSizes(i)*(ia_GrpSizes(i)-1))
        call rchkusr()
      END DO
       call rchkusr()
      d_DZ = SUM(da_C(1:i_NumGrps)*da_XI(1:i_NumGrps))
         call rchkusr()
      IF (d_DZ < d_DX) i_MP = i_MP + 1  ! resample Delta vs original Delta
         call rchkusr()
      IF (l_SAVETEST) THEN    ! Save value of test stat for this permuted sample
        call rchkusr()
         da_STV(i_IW+1) = d_DZ
      END IF
   END DO

   call rchkusr()
   IF (ALLOCATED(da_XI)) DEALLOCATE(da_XI,STAT=ios)
   da_Datas = da_DataHold
   IF (ALLOCATED(da_DataHold)) DEALLOCATE(da_DataHold,STAT=ios)
   d_Prob = DBLE(i_MP)/DBLE(i_NumPerms)



   ! END RESAMPLING estimation of P-Value.
ELSE
   ! BEGIN PEARSON TYPE III estimation of P-Value:
   ! da_XI1(I) contains average distance within ith group.
   ! Calculate d_DBar, the observed delta.
   d_DBar  = d_ZERO
   DO i=1,i_NumGrps
      da_XI1(i) = da_XI1(i)/DBLE(ia_GrpSizes(i))/DBLE(ia_GrpSizes(i)-1)
      d_DBar = d_DBar + da_C(i)*da_XI1(i)
      call rchkusr()
   END DO
   d_D3O = d_ZERO
   DO i=1,i_NumObs-2
      DO j=i+1,i_NumObs-1
         DO k=j+1,i_NumObs
            ! d_D3O = d_D3O + da_D(i,j)*da_D(i,k)*da_D(j,k)
            d_D3O = d_D3O + da_D(j+(2*i_NumObs-i)*(i-1)/2)*                   &
                            da_D(k+(2*i_NumObs-i)*(i-1)/2)*                   &
                            da_D(k+(2*i_NumObs-j)*(j-1)/2)
                            call rchkusr()
         END DO
      END DO
   END DO
   d_D3O = d_D3O*6.0D0/DBLE(i_NumObs)/DBLE(i_NumObs-1)/DBLE(i_NumObs-2)
   d_P1 = d_ZERO
   d_P2 = d_ZERO
   d_P3 = d_ZERO
   d_P4 = d_ZERO
   d_P1 = SUM(da_DSub1(1:i_NumObs)*da_DSub1(1:i_NumObs))
   d_P2 = SUM(da_DSub1(1:i_NumObs)*da_DSub2(1:i_NumObs))
   d_P4 = SUM(da_DSub1(1:i_NumObs)**3)
   DO i=1,i_NumObs-1
      DO j=i+1,i_NumObs
         d_P3 = d_P3 + da_DSub1(i)*da_DSub1(j)*da_D(j+(2*i_NumObs-i)*(i-1)/2)
         call rchkusr()
      END DO
   END DO
   ! END PEARSON TYPE III estimation of P-Value:
END IF
90000 CONTINUE
call rchkusr()
IF (ALLOCATED(da_XI)) DEALLOCATE(da_XI)

RETURN   ! ... normal exit ...
END SUBROUTINE mrppdist_1d

!=============================================================================!

SUBROUTINE sampwor(m, N, ID, da_Data, l_DataToZero)
!___Imported Parameters:
USE blcmnmod, ONLY: d_ZERO
        ! ^-- Some global Blossom variables and parameters.
USE mt19937, ONLY: genrand_real3
        ! ^-- Mersenne Twister Pseudo Random Number Generator
IMPLICIT NONE
!___Dummy Arguments:
INTEGER,          INTENT(IN)             :: m
INTEGER,          INTENT(IN)             :: N
INTEGER,          INTENT(IN)             :: ID
REAL (KIND(0D0)), INTENT(INOUT)          :: da_Data(:,:)
LOGICAL,          INTENT(IN),   OPTIONAL :: l_DataToZero
!x REAL (KIND(0D0)), INTENT(INOUT) :: da_U(:)
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     SUBROUTINE sampwor
! DESCRIPTION:
!     M: SIZE OF POPULATION
!     N: SIZE OF SAMPLE DRAWN WITHOUT REPLACEMENT (1 </= N </= M)
!     daDATA: REAL INPUT WHICH CONTAINS daDATA(1,*),...,daDATA(M,*)
!     POPULATION VALUES WITHOUT REPLACEMENT SAMPLE CONTAINED IN
!     daDATA(1,*),...,daDATA(N,*)
!     obsolete: uni(U) IS UNIFORM RANDOM NUMBER GENERATOR ON (0,1)
!     DIMENSION  da_U(17),d_Bin(51),da_Data(180,51)
! LANGUAGE:
!     Fortran90
! AUTHOR:
!     Dr. Paul W. Mielke, Jr., Dept of Statistics, Colorado State University
!        http://www.stat.colostate.edu/~mielke/mielkesoftware.html
!     Modifications:
!        Jon D. Richards, Operations Research Analyst    jon_richards@usgs.gov
!        Fort Collins Science Center                  http://www.fort.usgs.gov
!        U.S. Geological Survey  -  Biological Resources Division
! INVOKED BY:
!     mrppdist <File: "mrppmod.f90: mrppdist"> module
! INVOKES:
! (obsolete): FUNCTION uni RESULT(d_PPUni)<File: "mrppmod.f90: uni">  module

! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!___Local Variables:
REAL (KIND(0D0)) :: d_Bin
INTEGER :: i, j, k
!___Intrinsic Procedures:
INTRINSIC :: INT, PRESENT, RANDOM_NUMBER
!___Executable Statements:
DO i=1,N
call rchkusr()

      d_Bin = genrand_real3( )
   j = INT(i+(m-i+1)*d_Bin)
   !
   DO k=1,ID
      d_Bin = da_Data(j,k)
      da_Data(j,k) = da_Data(i,k)
      da_Data(i,k) = d_Bin
      call rchkusr()
   END DO
END DO
IF (PRESENT(l_DataToZero)) THEN
   IF (l_DataToZero) da_Data(1:N,ID+1) = d_ZERO
END IF
RETURN   ! ... normal exit ...
END SUBROUTINE sampwor

!=============================================================================!

FUNCTION uni(da_U, l_Init) RESULT(d_PPUni)
!___Imported Parameters and Variables:
USE blcmnmod, ONLY: d_ONE, d_ZERO
        ! ^-- Some global Blossom variables and parameters.
!___Imported Procedures:
!__(none)__
IMPLICIT NONE
!___Dummy Arguments:
REAL (KIND(0D0)), INTENT(INOUT)          :: da_U(:)
LOGICAL,          INTENT(IN),   OPTIONAL :: l_Init
REAL (KIND(0D0)) :: d_PPUni
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     FUNCTION uni RESULT(d_PPUni)
! DESCRIPTION:
!     uni(da_U) is uniform random number generator on (0,1)
!     Result is REAL (KIND(0D0)) d_PPUni
! LANGUAGE:
!     Fortran90
! AUTHOR:
!     Dr. Paul W. Mielke, Jr., Dept of Statistics, Colorado State University
!        http://www.stat.colostate.edu/~mielke/mielkesoftware.html
!     Modifications:
!        Jon D. Richards, Operations Research Analyst    jon_richards@usgs.gov
!        Fort Collins Science Center                  http://www.fort.usgs.gov
!        U.S. Geological Survey  -  Biological Resources Division
! INVOKED BY:
!     sampwor <File: "mrppmod.f90: mrppsampwor"> module
! INVOKES:
!     -

! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
REAL (KIND(0D0)), PARAMETER :: d_CSAVE = 2.1602869033813D-2
REAL (KIND(0D0)), PARAMETER ::    d_CD = 0.45623308420181D0
REAL (KIND(0D0)), PARAMETER ::    d_CM = 0.99999982118607D0

!___Local Variables:
REAL (KIND(0D0)) :: d_C
INTEGER i, j
SAVE i, j, d_C
DATA i, j, d_C/17,5,d_CSAVE/
!___Intrinsic Procedures:
INTRINSIC :: PRESENT
!___Executable Statements:
IF (PRESENT(l_Init)) THEN
   IF (l_Init) THEN
      i = 17
      j = 5
      d_C = d_CSAVE
   END IF
END IF
d_PPUni = da_U(i) - da_U(j)
IF (d_PPUni < d_ZERO) d_PPUni = d_PPUni + d_ONE
da_U(i) = d_PPUni
i = i - 1
IF (i == 0) i = 17
j = j - 1
IF (j == 0) j = 17
d_C = d_C - d_CD
IF (d_C < d_ZERO) d_C = d_C + d_CM
d_PPUni = d_PPUni - d_C
IF (d_PPUni < d_ZERO) d_PPUni = d_PPUni + d_ONE
RETURN   ! ... normal exit ...
END FUNCTION uni

!=============================================================================!

SUBROUTINE mrppcalc(i_NumObs, i_NumGrps, d_NumObs, ia_GrpSizes, d_D3O,        &
      d_DLit1, d_DLit2, d_DLit3, d_P1, d_P2, d_P3, d_P4, d_DBar, da_C, d_D1,  &
      d_Var, d_T, d_Gam)
                                       ! debug!
!___Imported Parameters and Variables:
USE blcmnmod, ONLY: d_ONE, d_TWO, d_ZERO
        ! ^-- Some global Blossom variables and parameters.
!___Imported Procedures:
USE jonsmodule, ONLY: lf_Equals
        ! ^-- lf_Equals: Generic test for equality of two objects.
IMPLICIT NONE
!___Dummy Arguments:
INTEGER,          INTENT(IN)  :: i_NumObs
INTEGER,          INTENT(IN)  :: i_NumGrps
REAL (KIND(0D0)), INTENT(IN)  :: d_NumObs
INTEGER,          INTENT(IN)  :: ia_GrpSizes(:) ! dim(i_NumGrps)
REAL (KIND(0D0)), INTENT(IN)  :: d_D3O
REAL (KIND(0D0)), INTENT(IN)  :: d_DLit1
REAL (KIND(0D0)), INTENT(IN)  :: d_DLit2
REAL (KIND(0D0)), INTENT(IN)  :: d_DLit3
REAL (KIND(0D0)), INTENT(IN)  :: d_P1
REAL (KIND(0D0)), INTENT(IN)  :: d_P2
REAL (KIND(0D0)), INTENT(IN)  :: d_P3
REAL (KIND(0D0)), INTENT(IN)  :: d_P4
REAL (KIND(0D0)), INTENT(IN)  :: d_DBar
REAL (KIND(0D0)), INTENT(IN)  :: da_C(:)   ! dim(i_NumGrps)
REAL (KIND(0D0)), INTENT(OUT) :: d_D1
REAL (KIND(0D0)), INTENT(OUT) :: d_Var
REAL (KIND(0D0)), INTENT(OUT) :: d_T
REAL (KIND(0D0)), INTENT(OUT) :: d_Gam
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     SUBROUTINE mrppcalc
! DESCRIPTION:
!     Calculates all the remaining parameters together with the first three
!     moments of the sampling distribution. This is needed for the Pearson
!     Type III distribution estimate of the P-Value for the MRPP.
! LANGUAGE:
!     Fortran 90/95
! AUTHOR:
!     Dr. Paul W. Mielke, Jr., Dept of Statistics, Colorado State University
!        http://www.stat.colostate.edu/~mielke/mielkesoftware.html
!     Modifications:
!        Jon D. Richards, Operations Research Analyst    jon_richards@usgs.gov
!        Fort Collins Science Center                  http://www.fort.usgs.gov
!        U.S. Geological Survey  -  Biological Resources Division
! INVOKED BY:
!     mrpp      <File: "mrppmod.f90: mrpp">         module
! INVOKES:
!     lf_Equals <File: "jonsmodule.f90: lf_Equals"> module

! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!___Local Variables:
REAL (KIND(0D0)) :: CJ, CJ2, CJ3
REAL (KIND(0D0)) :: COF1, COF2, COF3, COF4, COF5
REAL (KIND(0D0)) :: D3OO, D3SSS, D3OOO, D3SSSS
REAL (KIND(0D0)) :: d_D2, d_D3, D2S, D3S, D2SS, D3SS
REAL (KIND(0D0)) :: d_Expn
REAL (KIND(0D0)) :: d_SqRtVar
REAL (KIND(0D0)) :: SIJ, SIJ2, SIJ2Q
REAL (KIND(0D0)) :: SU2, SU3, SU4, SU5, SU6, SU7, SU8, SU9
INTEGER :: j
!___Intrinsic Procedures:
INTRINSIC :: DBLE, HUGE, SQRT

d_D1  = d_ZERO
d_Var = d_ZERO
d_T   = d_ZERO
d_Gam = d_ZERO
COF1   = (d_NumObs*(d_NumObs-d_ONE))
d_D1   = d_DLit1/COF1 ! d_D1 is the expected value of test stat (delta)
d_D2   = d_DLit2/COF1
d_D3   = d_DLit3/COF1
COF2   = COF1*DBLE(i_NumObs-2)
D2S    = (d_P1-d_DLit2)/COF2
D3S    = (d_P2-d_DLit3)/COF2
COF3   = COF2*DBLE(i_NumObs-3)
D2SS   = (d_DLit1*d_DLit1 - D2S*COF2*4.0D0 - d_DLit2*d_TWO)/COF3
D3SS   = (d_DLit1*d_DLit2 - D3S*COF2*4.0D0 - d_DLit3*d_TWO)/COF3
D3OO   = (d_P3*d_TWO - D3S*COF2*d_TWO - d_D3O*COF2 - d_DLit3)/COF3
D3OOO  = (d_P4 - D3S*COF2*3.0D0 - d_DLit3)/COF3
COF4   = COF3*DBLE(i_NumObs-4)
D3SSS  = (d_DLit1*D2S*COF2 - D3OO*COF3*4.0D0 - D3OOO*COF3*d_TWO -             &
            D3S*COF2*4.0D0 - d_D3O*COF2*d_TWO)/COF4
COF5   = COF4*DBLE(i_NumObs-5)
D3SSSS = (d_DLit1*D2SS*COF3 - D3SSS*COF4*8.0D0 - D3SS*COF3*4.0D0 -            &
            D3OO*COF3*8.0D0)/COF5
SU2 = d_ZERO
SU3 = d_ZERO
SU4 = d_ZERO
SU5 = d_ZERO
SU6 = d_ZERO
SU7 = d_ZERO
SU8 = d_ZERO
SU9 = d_ZERO
DO j=1,i_NumGrps
   SIJ   = DBLE(ia_GrpSizes(j))
   SIJ2  = SIJ*(SIJ-d_ONE)
   SIJ2Q = SIJ2*SIJ2
   CJ    = da_C(j)
   CJ2   = CJ*CJ
   CJ3   = CJ2*CJ
   SU2   = SU2 + CJ2/SIJ2
   SU3   = SU3 + CJ2/SIJ
   SU4   = SU4 + CJ3*4.0D0/SIJ2Q
   SU5   = SU5 + CJ3*(SIJ-d_TWO)*8.0D0/SIJ2Q
   SU6   = SU6 + CJ3*(SIJ-d_TWO)*(SIJ-3.0D0)*8.0D0/SIJ2Q
   SU7   = SU7 + CJ2*6.0D0*(d_ONE-CJ+CJ*(SIJ-d_TWO)*(SIJ-3.0D0)/SIJ2)/SIJ2
   SU8   = SU8 + CJ2*12.0D0*((d_ONE-CJ)*SIJ2*(SIJ-d_TWO) +                    &
               CJ*(SIJ-d_TWO)*(SIJ-3.0D0)*(SIJ-4.0D0))/SIJ2Q
   SU9   = SU9 +                                                              &
      CJ*((d_ONE-CJ)*(d_ONE-CJ*d_TWO)+CJ*(d_ONE-CJ)*(SIJ-d_TWO)*(SIJ-3.0D0)*  &
      3.0D0/SIJ2+CJ2*(SIJ-d_TWO)*(SIJ-3.0D0)*(SIJ-4.0D0)*(SIJ-5.0D0)/SIJ2Q)
      call rchkusr()
END DO
        ! d_Var is variance of the observe delta, d_Gam is skewness.
d_Var = (SU2-d_ONE/COF1)*(d_D2-D2S*d_TWO+D2SS)*d_TWO +                        &
      (SU3-d_ONE/d_NumObs)*(D2S-D2SS)*4.0D0
        ! If variance is negative, or very small, it is d_ZERO. If it is
        ! d_ZERO then d_Gam and d_T are not defined, and can't get PROB,
        ! so set these to very large values so Blossom knows what
        ! went on
IF (d_Var < d_ZERO) THEN
   d_Var = d_ZERO
   d_Gam = HUGE(99.99D99)
   d_T   = HUGE(99.99D99)
ELSE
   d_SqRtVar = SQRT(d_Var)
   d_Expn = d_Var*d_SqRtVar

   IF (lf_Equals(d_Expn, d_ZERO)) THEN !* <File: "jonsmodule.f90: lf_Equals">

      d_Gam = HUGE(99.99D99)
   ELSE
      d_Gam = ((SU4*d_D3+SU5*(D3S*3.0D0+d_D3O) + SU6*(D3OO*3.0D0+D3OOO)  +    &
            SU7*D3SS+SU8*D3SSS+SU9*D3SSSS) - d_D1*d_Var*3.0D0 -               &
            d_D1*d_D1*d_D1) / d_Expn
   END IF
   IF (lf_Equals(d_SqRtVar, d_ZERO)) THEN !* <File: "jonsmodule.f90: lf_Equals">
      d_T = HUGE(99.99D99)
   ELSE
      d_T   = (d_DBar-d_D1)/d_SqRtVar
   END IF
END IF

RETURN   ! ... normal exit ...
END SUBROUTINE mrppcalc

!=============================================================================!

SUBROUTINE emrpp(i_NumObs, i_NumVars, d_V, i_NumGrps, d_Sup, i_CForm, i_LZ,   &
      d_NInterv, ia_B_GrpSize, da_Datas, d_Delta1, da_YHot, d_PValue, iEr)
!d USE debugmod, ONLY: MyDebug                                         ! debug!
!___Imported Parameters and Variables:
USE blcmnmod, ONLY: d_ONE, d_TWO, d_ZERO, i_OK
        ! ^-- Some global Blossom variables and parameters.
USE jonsmodule, ONLY: i_CCode, i_MRPPMOD_ER004, i_MRPPMOD_ER005
        ! ^-- Message handling, etc.
USE mrolamod, ONLY: i_C_FORM_1, i_C_FORM_2, i_C_FORM_3, i_C_FORM_4,           &
      i_DO_HOTELLING, i_MAT_SINGULARITY
        ! ^-- Some high-level Blossom parameters.
!___Imported Procedures:
USE invertmod, ONLY: invert
        ! ^-- invert: matrix inversion routine.
USE jonsmodule, ONLY: lf_Equals
        ! ^-- lf_Equals: Generic test for equality of two objects.
IMPLICIT NONE
!___Dummy Arguments:
INTEGER,          INTENT(IN)    :: i_NumObs
INTEGER,          INTENT(IN)    :: i_NumVars
REAL (KIND(0D0)), INTENT(IN)    :: d_V
INTEGER,          INTENT(IN)    :: i_NumGrps
REAL (KIND(0D0)), INTENT(IN)    :: d_Sup
INTEGER,          INTENT(IN)    :: i_CForm
INTEGER,          INTENT(IN)    :: i_LZ
REAL (KIND(0D0)), INTENT(IN)    :: d_NInterv
INTEGER,          INTENT(INOUT) :: ia_B_GrpSize(:) ! (i_NumCase) check SIZE we use only i_NUmGrps
REAL (KIND(0D0)), INTENT(IN)    :: da_Datas(:,:)
REAL (KIND(0D0)), INTENT(OUT)   :: da_YHot(:,:)
REAL (KIND(0D0)), INTENT(OUT)   :: d_Delta1
REAL (KIND(0D0)), INTENT(OUT)   :: d_PValue
INTEGER,          INTENT(OUT)   :: iEr
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     SUBROUTINE emrpp
! DESCRIPTION:
!     Exact Multi-Response Permutation Procedures
!     This code was written by Kenneth J. Berry and Paul W. Mielke, Jr.
!                            Department of Statistics
!                            Colorado State University
!                            Fort Collins, Colorado 80523
!         .  .  .  .  .  .  .  .  .  .  .  .
!  Dummy Argument Description:
!                           (IN) i_NumObs     number of observations (cases)
!                           (IN) i_NumVars    number of response variables
!                           (IN) d_V          distance exponent (default=1.0)
!                           (IN) i_NumGrps    number of groups
!                           (IN) d_Sup        truncation distance
!                           (IN) i_CForm      weighting form
!                           (IN) i_LZ         Hotelling's Commens. (1=Y,0=N)
!                           (IN) d_NInterv    number of intervals in cycle
! dim(i_NumCased)          (INOUT) ia_B_GrpSize group sizes (use only i_NumGrps)
! dim(i_NumObs,i_NumVars+1)   (IN) da_Datas     observation data
! dim(i_NumVars,i_NumVars)   (OUT) da_YHot      variance/covariance matrix
!                          (OUT) d_Delta1     observed MRPP statistic
!                          (OUT) d_PValue     exact MRPP P-value
!                          (OUT) iEr         error code (0=OK)
!         .  .  .  .  .  .  .  .  .  .  .  .
! LANGUAGE:
!     Fortran 90, with Lahey Computer Systems LF90 intrinsics extensions
! AUTHOR:
!     Dr. Paul W. Mielke, Jr., Dept of Statistics, Colorado State University
!        http://www.stat.colostate.edu/~mielke/mielkesoftware.html
!     Modifications:
!        Jon D. Richards, Operations Research Analyst    jon_richards@usgs.gov
!        Fort Collins Science Center                  http://www.fort.usgs.gov
!        U.S. Geological Survey  -  Biological Resources Division
! INVOKED BY:
!     runemrpp  <File: "domrpp.f90: runemrpp">      module
! INVOKES:
!     emrppperm <File: "mrppmod.f90: emrppperm">    module
!     invert    <File: "invrtmod.f90: invert">      module
!     lf_Equals <File: "jonsmodule.f90: lf_Equals"> module

!___Local Variables:
REAL (KIND(0D0)), ALLOCATABLE :: da_YHotHold(:,:)
REAL (KIND(0D0)), ALLOCATABLE :: da_C(:)
REAL (KIND(0D0)), ALLOCATABLE :: da_D(:)
REAL (KIND(0D0)), ALLOCATABLE :: da_S(:)
REAL (KIND(0D0)), ALLOCATABLE :: da_UHot(:)
REAL (KIND(0D0)) :: d_NumObs, d_Sum, d_YTemp
INTEGER :: i, j, l, k
INTEGER :: ios
INTEGER :: i_Kount1, i_Kount2, i_MSize
INTEGER :: i_MyL
!___Intrinsic Procedures:
INTRINSIC :: ABS, ALLOCATED, DBLE, MIN, SUM
!___Executable Statements:

iEr = i_OK ! no problem yet
da_YHot  = d_ZERO
d_PValue = d_ZERO
i_MSize  = i_NumObs*(i_NumObs-1)/2 + i_NumObs
d_NumObs = DBLE(i_NumObs)

ALLOCATE(                &
      da_S(1:i_NumGrps), &
      da_C(1:i_NumGrps), &
      da_D(1:i_MSize),   & ! IS THIS CORRECT? - JDR 01-95
      STAT=ios)
IF (ios /= 0) THEN
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |_________________
!  |                                                    |
!  | 'Memory allocation error: da_S,da_C,da_D in EMRPP' |
!  |____________________________________________________|
!
   i_CCode = ios
   iEr = i_MRPPMOD_ER004
   RETURN   ! ... error exit ...
END IF



l = SUM(ia_B_GrpSize(1:i_NumGrps))
ia_B_GrpSize(i_NumGrps+1) = i_NumObs - l
DO i=1,i_NumGrps
   da_S(i) = DBLE(ia_B_GrpSize(i)*(ia_B_GrpSize(i)-1))/d_TWO
   call rchkusr()
END DO
SELECT CASE (i_CForm)
   CASE (i_C_FORM_1)
      DO i=1,i_NumGrps
         da_C(i) = DBLE(ia_B_GrpSize(i))/DBLE(l)
      END DO
   CASE (i_C_FORM_2)
      DO i=1,i_NumGrps
         da_C(i) = DBLE(ia_B_GrpSize(i)-1)/DBLE(l-i_NumGrps)
      END DO
   CASE (i_C_FORM_3)
      DO i=1,i_NumGrps
         da_C(i) = d_ONE/DBLE(i_NumGrps)
      END DO
   CASE (i_C_FORM_4)
      d_YTemp = d_ZERO
      DO i=1,i_NumGrps
         d_YTemp = d_YTemp + DBLE(ia_B_GrpSize(i)*(ia_B_GrpSize(i)-1))
      END DO
      DO i=1,i_NumGrps
         da_C(i) = DBLE(ia_B_GrpSize(i)*(ia_B_GrpSize(i)-1))/d_YTemp
      END DO
END SELECT
!
l = 1
IF (i_LZ == i_DO_HOTELLING) THEN           ! Hotelling's commensuration
   ALLOCATE(                                &
      da_YHotHold(1:i_NumVars,1:i_NumVars), &
          da_UHot(1:i_NumVars),             &
      STAT=ios)
   IF (ios /= i_OK) THEN
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |______________________
!  |                                                         |
!  | 'Memory allocation error: da_YHotHold,da_UHot in EMRPP' |
!  |_________________________________________________________|
!
      DEALLOCATE(da_S, da_C, da_D, STAT=ios)
      i_CCode = ios
      iEr = i_MRPPMOD_ER005
      RETURN   ! ... error exit ...
   END IF
   DO i=1,i_NumVars                 ! for each variable...
      da_UHot(i) = SUM(da_Datas(1:i_NumObs,i))
      da_UHot(i) = da_UHot(i) / d_NumObs !...get its average
   END DO
   DO i=1,i_NumVars
      DO j=1,i_NumVars
         da_YHot(i,j) = SUM((da_Datas(1:i_NumObs,i)-da_UHot(i)) *  &
                            (da_Datas(1:i_NumObs,j)-da_UHot(j)))
                            call rchkusr()
      END DO
   END DO
   DEALLOCATE(da_UHot, STAT=ios)
   da_YHotHold = da_YHot ! Save un-inverted var/cov matrix.
   CALL invert(i_NumVars, da_YHot, iEr) !* <File: "invrtmod.f90: invert">
   IF (iEr /= i_OK) THEN
      ! IF (iEr == i_MAT_SINGULARITY) THEN
!   _____________________________________________________
!  |                                                     |
!  | 'Either all the elements of some row were zero or'  |
!  | 'a pivot became relatively zero.'                   |
!  | 'The variance/covariance matrix of variable values' |
!  | 'is probably singular and cannot be inverted.'      |
!  | 'Hotelling''s commensuration can''t be done.'       |
!  |_____________________________________________________|
!
      ! END IF
      da_YHot = da_YHotHold ! Return un-inverted var/cov matrix to caller.
      DEALLOCATE(da_S, da_C, da_D, da_YHotHold, STAT=ios)
      RETURN   ! ... error exit ...
   END IF
END IF
da_D = d_ZERO
DO i=1,i_NumObs-1    ! compute the distances
   DO j=i+1,i_NumObs
      d_Sum = d_ZERO
      IF (lf_Equals(d_NInterv, d_ZERO)) THEN ! distance calc for non-ARC data:

         DO k=1,i_NumVars
            IF (i_LZ /= i_DO_HOTELLING) THEN
               d_Sum = d_Sum + (da_Datas(i,k)-da_Datas(j,k))**2
            ELSE   ! Hotelling's adjustment
               DO i_MyL=1,i_NumVars
                  d_Sum = d_Sum + (da_Datas(i,k)-da_Datas(j,k)) *             &
                                       da_YHot(k,i_MyL) *                     &
                              (da_Datas(i,i_MyL)-da_Datas(j,i_MyL))
                              call rchkusr()
               END DO
            END IF
         END DO

         da_D(l) = d_Sum**(d_V/d_TWO) ! adjust for distance exponent
      ELSE  ! distance calc for ARC data (univariate only):
         d_Sum = MIN( ABS(da_Datas(i,1)-da_Datas(j,1)),                       &
               d_NInterv-ABS(da_Datas(i,1)-da_Datas(j,1)) )

         da_D(l) = d_Sum**d_V
      END IF
         ! if cutoff distance is positive, then use it
      IF (d_Sup > d_ZERO) then
         IF (da_D(l) > d_Sup) da_D(l) = d_Sup
      END IF
      l = l + 1
      call rchkusr()
   END DO
END DO
IF (ALLOCATED(da_YHotHold)) THEN
   da_YHot = da_YHotHold ! Return un-inverted var/cov matrix to caller.
   DEALLOCATE(da_YHotHold, STAT=ios)
END IF

CALL emrppperm(i_NumObs,     & !* <File: "mrppmod.f90: emrppperm">
               i_NumGrps,    &
               ia_B_GrpSize, &
               da_S,         &
               da_C,         &
               da_D,         &
               i_Kount1,     &
               i_Kount2,     &
               d_Delta1,     &
               iEr)
IF (iEr == i_OK) THEN
   d_PValue = DBLE(i_Kount2)/DBLE(i_Kount1)
END IF

DEALLOCATE(da_S, da_C, da_D, STAT=ios)

RETURN   ! ... normal exit ...
END SUBROUTINE emrpp

!=============================================================================!

SUBROUTINE emrppperm(i_NumObs, i_NumGrps, ia_B_GrpSize, da_S, da_C, da_D,     &
      i_Kount1, i_Kount2, d_Delta1, iEr)

!___Imported Parameters and Variables:
USE blcmnmod, ONLY: i_OK
        ! ^-- Some global Blossom variables and parameters.
USE jonsmodule, ONLY: i_CCode, i_MRPPMOD_ER006
        ! ^-- Message handling, etc.
!___Imported Procedures:
!__(none)__
IMPLICIT NONE
!___Dummy Arguments:
INTEGER,          INTENT(IN)  :: i_NumObs
INTEGER,          INTENT(IN)  :: i_NumGrps
INTEGER,          INTENT(IN)  :: ia_B_GrpSize(:)
REAL (KIND(0D0)), INTENT(IN)  :: da_S(:)
REAL (KIND(0D0)), INTENT(IN)  :: da_C(:)
REAL (KIND(0D0)), INTENT(IN)  :: da_D(:)
INTEGER,          INTENT(OUT) :: i_Kount1
INTEGER,          INTENT(OUT) :: i_Kount2
REAL (KIND(0D0)), INTENT(OUT) :: d_Delta1
INTEGER,          INTENT(OUT) :: iEr
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     SUBROUTINE emrppperm
! DESCRIPTION:
!     Computations for Exact Multi-Response Permutation Procedures program.
! LANGUAGE:
!     Fortran 90/95
! AUTHOR:
!     Dr. Paul W. Mielke, Jr., Dept of Statistics, Colorado State University
!        http://www.stat.colostate.edu/~mielke/mielkesoftware.html
!     Modifications:
!        Jon D. Richards, Operations Research Analyst    jon_richards@usgs.gov
!        Fort Collins Science Center                  http://www.fort.usgs.gov
!        U.S. Geological Survey  -  Biological Resources Division
! INVOKED BY:
!     emrpp     <File: "mrppmod.f90: emrpp">     module
! INVOKES:
!     emrppdval <File: "mrppmod.f90: emrppdval"> module
!
!___Local Variables:
REAL (KIND(0D0)), ALLOCATABLE :: da_XI(:) ! SIZE=i_NumGrps
INTEGER, ALLOCATABLE :: ia_A(:,:)
INTEGER, ALLOCATABLE :: ia_P(:)
INTEGER :: i, j, k, l, m, ii, jj, ll
INTEGER :: ios, ios2
INTEGER :: i_Flag, i_Limit, i_NMarks, i_Temp

iEr = i_OK ! no problem yet
ALLOCATE(                             &
      ia_A(1:i_NumGrps+1,1:i_NumObs), &
      ia_P(1:i_NumObs),               &
     da_XI(1:i_NumGrps),              &
      STAT=ios)
IF (ios /= i_OK) THEN
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |________________
!  |                                                   |
!  | 'Memory allocation error: ia_A,ia_P in EMRPPPERM' |
!  |___________________________________________________|
!
   i_CCode = ios
   iEr = i_MRPPMOD_ER006
   RETURN   ! ... error exit ...
END IF
k = i_NumGrps
IF (ia_B_GrpSize(i_NumGrps+1) /= 0) k = k + 1
l = i_NumObs
DO i=1,k-1
   DO j=1,ia_B_GrpSize(i)
      ia_A(i,j) = i
   END DO
   DO j=ia_B_GrpSize(i)+1,l
      ia_A(i,j) = i + 1
   END DO
   l = l - ia_B_GrpSize(i)
   call rchkusr()
END DO
i_Kount1 = 0
i_Kount2 = 0

bigloop: DO
   i = k - 1
   i_NMarks = ia_B_GrpSize(k) + ia_B_GrpSize(i)
   IF (i_Kount1 == 0) GOTO 130

   loop70: DO
      DO j=1,i_NMarks-1
         IF (ia_A(i,j) == i) THEN
            IF (ia_A(i,j+1) == i+1) THEN
               i_Limit     = j - 2
               ia_A(i,j)   = ia_A(i,j+1)
               ia_A(i,j+1) = ia_A(i,j) - 1
               IF (i_Limit <= 0) GOTO 130

               EXIT loop70
            END IF
         END IF
         call rchkusr()
      END DO
      IF (i_NMarks /= 1) THEN
         DO j=1,i_NMarks/2
            i_Temp    = ia_A(i,j)
            ia_A(i,j) = ia_A(i,i_NMarks-j+1)
            ia_A(i,i_NMarks-j+1) = i_Temp
            call rchkusr()
         END DO
      END IF

      i = i - 1
      IF (i == 0) EXIT bigloop
      i_NMarks = i_NMarks + ia_B_GrpSize(i)

   END DO loop70

   loop110: DO
      i_Flag = 0
      DO j=1,i_Limit
         IF (ia_A(i,j) == i+1) THEN
            IF (ia_A(i,j+1) == i) THEN
               ia_A(i,j)   = ia_A(i,j+1)
               ia_A(i,j+1) = ia_A(i,j) + 1
               i_Flag = 1
            END IF
         END IF
         call rchkusr()
      END DO

      IF (i_Flag == 1) THEN
         CYCLE
      ELSE
         EXIT
      END IF
      call rchkusr()
   END DO loop110
   130   CONTINUE
   ia_A(k,1:i_NumObs) = ia_A(1,1:i_NumObs)

   IF (k /= 2) THEN
      DO l=2,k-1
         m = 1
         DO j=1,i_NumObs
            IF (ia_A(k,j) == l) THEN
               ia_A(k,j) = ia_A(l,m)
               m = m + 1
            END IF
            call rchkusr()
         END DO
      END DO
   END IF

   ll = 1
   DO ii=1,k
      DO jj=1,i_NumObs
         IF (ia_A(k,jj) == ii) THEN
            ia_P(ll) = jj
            ll = ll + 1
         END IF
         call rchkusr()
      END DO
   END DO
   CALL emrppdval(i_NumObs, i_NumGrps, ia_B_GrpSize, da_S, da_C, ia_P, da_D,  &
         i_Kount1, i_Kount2, da_XI, d_Delta1, iEr)
   IF (iEr /= i_OK) EXIT bigloop
   i_Kount1 = i_Kount1 + 1

END DO bigloop
90000 CONTINUE

DEALLOCATE(ia_A, ia_P, da_XI, STAT=ios2)
RETURN   ! ... normal exit ...
END SUBROUTINE emrppperm

!=============================================================================!

SUBROUTINE emrppdval(i_NumObs, i_NumGrps, ia_B_GrpSize, da_S, da_C, ia_P,     &
      da_D, i_Kount1, i_Kount2, da_XI, d_Delta1, iEr)

!___Imported Parameters and Variables:
USE blcmnmod, ONLY: d_ONE, d_ZERO, i_OK
        ! ^-- Some global Blossom variables and parameters.
! USE jonsmodule, ONLY: i_CCode, i_MRPPMOD_ER007
!         ! ^-- Message handling, etc.
!___Imported Procedures:
!__(none)__
IMPLICIT NONE
!___Dummy Arguments:
INTEGER,          INTENT(IN)    :: i_NumObs
INTEGER,          INTENT(IN)    :: i_NumGrps
INTEGER,          INTENT(IN)    :: ia_B_GrpSize(:)
REAL (KIND(0D0)), INTENT(IN)    :: da_S(:)
REAL (KIND(0D0)), INTENT(IN)    :: da_C(:)
INTEGER,          INTENT(IN)    :: ia_P(:)
REAL (KIND(0D0)), INTENT(IN)    :: da_D(:)
INTEGER,          INTENT(IN)    :: i_Kount1
INTEGER,          INTENT(INOUT) :: i_Kount2
REAL (KIND(0D0)), INTENT(INOUT) :: da_XI(:)
REAL (KIND(0D0)), INTENT(INOUT) :: d_Delta1
INTEGER,          INTENT(OUT)   :: iEr
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     SUBROUTINE emrppdval
! DESCRIPTION:
!     Computations for Exact Multi-Response Permutation Procedures program.
! LANGUAGE:
!     Fortran 90/95
! AUTHOR:
!     Dr. Paul W. Mielke, Jr., Dept of Statistics, Colorado State University
!        http://www.stat.colostate.edu/~mielke/mielkesoftware.html
!     Modifications:
!        Jon D. Richards, Operations Research Analyst    jon_richards@usgs.gov
!        Fort Collins Science Center                  http://www.fort.usgs.gov
!        U.S. Geological Survey  -  Biological Resources Division
! INVOKED BY:
!     emrppperm <File: "mrppmod.f90: emrppperm"> module
! INVOKES:
!     -
! FILES:
!     -

!___Procedure Parameters:
REAL (KIND(0D0)), PARAMETER :: d_PREC = 1.0D-12
!___Local Variables:

REAL (KIND(0D0)) :: d_Delta
INTEGER :: i, j, k, ii, kk, ll, iii, jjj
! INTEGER :: ios, ios2
!___Intrinsice Procedures:
INTRINSIC :: SUM
!___Executable Statements:
iEr = i_OK ! no problem yet

! IF (ios /= i_OK) THEN
! !   __________________________________
! !  |                                  |
! !  | IOSTAT_MSG(i_Code) error message |____________
! !  |                                               |
! !  | 'Memory allocation error: da_XI in EMRPPDVAL' |
! !  |_______________________________________________|
! !
!    i_CCode = ios
!    iEr = i_MRPPMOD_ER007
!    RETURN   ! ... error exit ...
! END IF
ll = 0
DO ii=1,i_NumGrps
   kk = ll + 1
   ll = ll + ia_B_GrpSize(ii)
   da_XI(ii) = d_ZERO
   DO iii=kk,ll-1
      i = ia_P(iii)
      DO jjj=iii+1,ll
         j = ia_P(jjj)
         k = i_NumObs*(i-1) + j - (i*(i+1))/2
         da_XI(ii) = da_XI(ii) + da_D(k)
         call rchkusr()
      END DO
   END DO
   da_XI(ii) = da_XI(ii) / da_S(ii)
END DO
d_Delta = SUM(da_C(1:i_NumGrps)*da_XI(1:i_NumGrps))
IF (i_Kount1 == 0) d_Delta1 = (d_ONE+d_PREC)*d_Delta

IF (d_Delta <= d_Delta1) i_Kount2 = i_Kount2 + 1

RETURN   ! ... normal exit ...
END SUBROUTINE emrppdval

!=============================================================================!

SUBROUTINE ptmp(i_NumPairs, d_V, i_L, d_H, da_D1, da_D2, d_Expected_Delta,  &
      d_Var, d_Gam, d_T, d_Rho, d_Delta1, d_PValue, l_ExactDone, iEr)
!___Imported Parameters and Variables:
USE blcmnmod, ONLY: d_ONE, d_TWO, d_ZERO, i_OK
        ! ^-- Some global Blossom variables and parameters.
USE ioformod, ONLY: ch12_OutBuff
        ! ^-- Buffers for character output of numbers.
USE jonsmodule, ONLY: i_CCode, i_MRPPMOD_ER008, i_MRPPMOD_ER009
        ! ^-- Message handling, etc.
USE mrolamod, ONLY: i_MIN_NUM_PTMP_CASES, i_NUM2DO_EPTMP
        ! ^-- Some high-level Blossom parameters.
!___Imported Procedures:
! USE jonsmodule, ONLY: lf_Equals
!         ! ^-- lf_Equals: Generic test for equality of two objects.
USE pv_modul, ONLY: pvalue
        ! ^-- pvalue: compute P-value
IMPLICIT NONE
!___Dummy Arguments:
INTEGER,          INTENT(IN)  :: i_NumPairs ! number of pairs (in)
REAL (KIND(0D0)), INTENT(IN)  :: d_V  ! distance exponent
INTEGER,          INTENT(IN)  :: i_L  ! power of ranks? 1=Y,2=N
REAL (KIND(0D0)), INTENT(IN)  :: d_H  ! exponent for power of ranks
REAL (KIND(0D0)), INTENT(IN)  :: da_D1(:) ! distances of 1st in pair
REAL (KIND(0D0)), INTENT(IN)  :: da_D2(:) ! distances of 2nd in pair
REAL (KIND(0D0)), INTENT(OUT) :: d_Expected_Delta ! expected delta
REAL (KIND(0D0)), INTENT(OUT) :: d_Var    ! variance of delta
REAL (KIND(0D0)), INTENT(OUT) :: d_Gam    ! skewness of delta
REAL (KIND(0D0)), INTENT(OUT) :: d_T      ! test stat.
REAL (KIND(0D0)), INTENT(OUT) :: d_Rho    ! agreement measure
REAL (KIND(0D0)), INTENT(OUT) :: d_Delta1 ! observed value of delta
REAL (KIND(0D0)), INTENT(OUT) :: d_PValue ! pvalue
LOGICAL,          INTENT(IN) :: l_ExactDone ! true if eptmp
INTEGER,          INTENT(OUT) :: iEr
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     SUBROUTINE ptmp
! DESCRIPTION:
!     Permutation test for matched pairs (PTMP) yields either
!     exact (N = 3,...,20) or approximate (N = 21,...)
!     p-values (power of ranks test is included as a special
!     case).  Blossom does not use the power of ranks test option.
!         .  .  .  .  .  .  .  .  .  .  .  .
!  Dummy Argument Description:
!                 (IN) i_NumPairs       number of pairs (in)
!                 (IN) d_V              distance exponent
!                 (IN) i_L               power of ranks? 1=Y,2=N
!                 (IN) d_H              exponent for power of ranks
! dim(i_NumPairs)  (IN) da_D1            distances of 1st in pair
! dim(i_NumPairs)  (IN) da_D2            distances of 2nd in pair
!                (OUT) d_Expected_Delta  expected delta
!                (OUT) d_Var            variance of delta
!                (OUT) d_Gam            skewness of delta
!                (OUT) d_T              observed standardized test statistic
!                (OUT) d_Rho            agreement measure
!                (OUT) d_Delta1         observed value of delta
!                (OUT) d_PValue         p-value smaller or equal delta
!                (OUT) i_NumNonZeroDiff actual # pairs of non-zero differences
!                (OUT) l_ExactPTMPDone  true if exact ptmp (i_NumPairs>20)
!                (OUT) iEr             error code

!-------------------------------
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!___Program Parameters:
INTEGER, PARAMETER :: i_PWR_RANKS_YES = 2
!___Local Variables:

REAL (KIND(0D0)), ALLOCATABLE :: da_A(:)
REAL (KIND(0D0)), ALLOCATABLE :: da_B(:)
REAL (KIND(0D0)), ALLOCATABLE :: da_R(:)
REAL (KIND(0D0)), ALLOCATABLE :: da_X(:)
REAL (KIND(0D0)), ALLOCATABLE :: da_Y(:)
REAL (KIND(0D0)) :: d_AS, d_B1, d_B2, d_BS, d_CS, d_Sum
REAL (KIND(0D0)) :: d_A1, d_A2, d_A3, d_A4, d_Diff, d_FN
REAL (KIND(0D0)) :: d_D1, d_D2, d_Phi, d_W
INTEGER :: i, j, k
INTEGER :: ip, jp, ix
INTEGER :: ios ! error status flag
!___Intrinsic Procedures:
INTRINSIC :: ABS, DBLE, SQRT
!___Statement Functions:
INTEGER :: dispos
dispos(i, j) = i + (2*i_NumPairs-j)*(j-1)/2

iEr = 0 ! no problem yet

ALLOCATE(                              &
      da_X(1:i_NumPairs),              &
      da_Y(1:i_NumPairs),              &
      da_R(1:i_NumPairs),              &

      da_A(1:i_NumPairs+i_NumPairs*(i_NumPairs-1)/2), &
      da_B(1:i_NumPairs+i_NumPairs*(i_NumPairs-1)/2), &
      STAT=ios)
IF (ios /= 0) THEN
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |__________________________
!  |                                                             |
!  | 'Memory allocation error: da_X,da_Y,da_R,da_A,da_B in PTMP' |
!  |_____________________________________________________________|
!
   DEALLOCATE(da_X, da_Y, da_R, da_A, da_B, STAT=ios)

   i_CCode = ios
   iEr = i_MRPPMOD_ER008
   RETURN   ! ... error exit ...
END IF

da_X = da_D1 - da_D2

IF (i_NumPairs < i_MIN_NUM_PTMP_CASES) THEN
!   __________________________________________
!  |                                          |
!  | (i_NumNonZeroDiff, i_MIN_NUM_PTMP_CASES) |_________________________
!  |                                                                    |
!  | 'There are $1 non-zero differences in your data.                   |
!  | There must be at least $2 non-zero differences to do a Permutation |
!  | Test for Matched Pairs'                                            |
!  |____________________________________________________________________|
!
   DEALLOCATE(da_X, da_Y, da_R, da_A, da_B, STAT=ios)
   iEr = i_MRPPMOD_ER009

   RETURN   ! ... error exit ...
END IF
IF (i_L == i_PWR_RANKS_YES) THEN   ! BEGIN POWER OF RANKS . . .

   da_Y = ABS(da_X)
   d_Phi = d_ONE + 1.0D-12
   d_A1  = d_ZERO
   d_A2  = d_ZERO
   d_A3  = DBLE(i_NumPairs) - 0.1D0
   d_B1  = d_ZERO
   d_B2  = 1.0D30
   DO WHILE(d_A2 <= d_A3)

      DO i=1,i_NumPairs
         IF (da_Y(i) > d_B1) THEN
            IF (da_Y(i) < d_B2) THEN
               d_B2 = da_Y(i)*d_Phi
            END IF
         END IF
         call rchkusr()
      END DO
      DO i=1,i_NumPairs
         d_W = ABS(d_ONE-da_Y(i)/d_B2)
         IF (d_W < 1.0D-11) d_A1 = d_A1 + d_ONE
         call rchkusr()
      END DO
      d_A4 = d_A2 + (d_A1+d_ONE)/d_TWO
      DO i=1,i_NumPairs
         d_W = ABS(d_ONE-da_Y(i)/d_B2)
         IF (d_W < 1.0D-11) da_R(i) = d_A4
         call rchkusr()
      END DO
      d_A2 = d_A2 + d_A1
      d_A1 = d_ZERO
      d_B1 = d_B2
      d_B2 = 1.0D30

   END DO

   da_X = (da_X/da_Y)*da_R**d_H

END IF   ! END POWER OF RANKS . . .

d_FN = DBLE(i_NumPairs)
IF (l_ExactDone .eqv. .TRUE.) THEN   ! do EXACT PTMP if <= 20 cases

      CALL ptmpcomb(i_NumPairs, d_FN, d_V, da_X, d_Delta1, d_PValue)

ELSE      ! NOT exact test, but approximation from observed data
   d_Sum = d_ZERO

   DO i=1,i_NumPairs-1

      DO j=i+1,i_NumPairs
         d_Sum = d_Sum + (ABS(da_X(i)-da_X(j)))**d_V
         call rchkusr()
      END DO
   END DO
   d_Delta1 = d_Sum*d_TWO/(d_FN*(d_FN-d_ONE))

   da_X = ABS(da_X)
   d_AS = d_ZERO
   d_BS = d_ZERO
   d_CS = d_ZERO

   DO i=1,i_NumPairs-1
      DO j=i+1,i_NumPairs
         IF (j <= i) THEN
            ip = i
            jp = j
         ELSE
            ip = j
            jp = i
         END IF
         ix = dispos(ip, jp)

         da_A(ix) = (da_X(i)+da_X(j))**d_V
         da_B(ix) = (ABS(da_X(i)-da_X(j)))**d_V
         d_AS = d_AS + (da_A(ix)+da_B(ix))
         d_BS = d_BS + (da_A(ix)-da_B(ix)) * (da_A(ix)-da_B(ix))
         call rchkusr()
      END DO
   END DO

   DO i=1,i_NumPairs-2
      DO j=i+1,i_NumPairs-1
         DO k=j+1,i_NumPairs

            d_CS = d_CS + &
      (da_A(j+(2*i_NumPairs-i)*(i-1)/2)-da_B(j+(2*i_NumPairs-i)*(i-1)/2)) * &
      (da_A(k+(2*i_NumPairs-i)*(i-1)/2)-da_B(k+(2*i_NumPairs-i)*(i-1)/2)) * &
      (da_A(k+(2*i_NumPairs-j)*(j-1)/2)-da_B(k+(2*i_NumPairs-j)*(j-1)/2))
      call rchkusr()
         END DO
      END DO
   END DO
   d_Expected_Delta = d_AS / (d_FN*(d_FN-d_ONE))
   d_Var    = d_BS / (d_FN*d_FN*(d_FN-d_ONE)*(d_FN-d_ONE))
   d_T      = (d_Delta1-d_Expected_Delta) / SQRT(d_Var)
   d_Gam    = -d_CS*6.0D0 / (d_BS*SQRT(d_BS))
   d_Rho    = d_ONE - d_Delta1/d_Expected_Delta
   d_PValue = pvalue(d_T, d_Gam)  ! from PV_MODULE  ! what about EXACT TEST ?

END IF


DEALLOCATE(da_X, da_Y, da_R, da_A, da_B, STAT=ios)
RETURN   ! ... normal exit ...
END SUBROUTINE ptmp

!=============================================================================!

SUBROUTINE ptmpcomb(i_NumPairs, d_FN, d_V, da_X, d_Delta1, d_PValue)
!___Imported Parameters and Variables:
USE blcmnmod, ONLY: d_ZERO
        ! ^-- Some global Blossom variables and parameters.
!___Imported Procedures:
!__(none)__
IMPLICIT NONE
!___Dummy Arguments:
INTEGER,          INTENT(IN)  :: i_NumPairs
REAL (KIND(0D0)), INTENT(IN)  :: d_FN
REAL (KIND(0D0)), INTENT(IN)  :: d_V
REAL (KIND(0D0)), INTENT(IN)  :: da_X(:)
REAL (KIND(0D0)), INTENT(OUT) :: d_Delta1
REAL (KIND(0D0)), INTENT(OUT) :: d_PValue
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     SUBROUTINE ptmpcomb
! DESCRIPTION:
!     Compute combinations and counts for PTMP.
! LANGUAGE:
!     Fortran 90/95
! AUTHOR:
!     Dr. Paul W. Mielke, Jr., Dept of Statistics, Colorado State University
!        http://www.stat.colostate.edu/~mielke/mielkesoftware.html
!     Modifications:
!        Jon D. Richards, Operations Research Analyst    jon_richards@usgs.gov
!        Fort Collins Science Center                  http://www.fort.usgs.gov
!        U.S. Geological Survey  -  Biological Resources Division
! INVOKED BY:
!     ptmp     <File: "mrppmod.f90: ptmp">     module
! INVOKES:
!     ptmpxact <File: "mrppmod.f90: ptmpxact"> module
! FILES:
!     -

! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!___Local Variables:
INTEGER :: ia_M(20)
INTEGER :: j
INTEGER :: i_Kount1, i_Kount2, i_Kount3, i_Last, i_NMinus
LOGICAL :: l_First
!___Intrinsic Procedures:
INTRINSIC :: DBLE, MOD
!___Executable Statements:
ia_M     = 1 ! Initialize array to 1
i_Kount1 = 0
i_Kount2 = 0
i_Kount3 = 0
i_NMinus = 0
i_Last   = 0
l_First  = .TRUE.
DO
   IF (.NOT.l_First) THEN
      j = 0
      DO
         j = j + 1
         IF (MOD(i_Kount3, 2) == 1) EXIT
         i_Kount3 = i_Kount3 / 2
         call rchkusr()
      END DO
      i_Last   = ia_M(j)
      ia_M(j)  = -i_Last
   END IF
   l_First  = .FALSE.
   i_Kount1 = i_Kount1 + 1
   i_Kount3 = i_Kount1
   i_NMinus = i_NMinus + i_Last
   CALL ptmpxact(i_NumPairs, & !* <File: "mrppmod.f90: ptmpxact">
                 d_FN,       &
                 d_V,        &
                 da_X,       &
                 ia_M,       &
                 i_Kount1,   &
                 i_Kount2,   &
                 d_Delta1)
   IF (i_NMinus == 1) THEN
      IF (ia_M(i_NumPairs) == -1) EXIT
   END IF
   call rchkusr()
END DO
d_PValue = DBLE(i_Kount2)/DBLE(i_Kount1)

RETURN   ! ... normal exit ...
END SUBROUTINE ptmpcomb

!=============================================================================!

SUBROUTINE ptmpxact(i_NumPairs, d_FN, d_V, da_X, ia_M, i_Kount1, i_Kount2, &
      d_Delta1)
!___Imported Parameters and Variables:
USE blcmnmod, ONLY: d_ONE, d_TWO, d_ZERO
        ! ^-- Some global Blossom variables and parameters.
!___Imported Procedures:
!__(none)__
IMPLICIT NONE
!___Dummy Arguments:
INTEGER,          INTENT(IN)    :: i_NumPairs
REAL (KIND(0D0)), INTENT(IN)    :: d_FN
REAL (KIND(0D0)), INTENT(IN)    :: d_V
REAL (KIND(0D0)), INTENT(IN)    :: da_X(:)
INTEGER,          INTENT(IN)    :: ia_M(:)
INTEGER,          INTENT(IN)    :: i_Kount1
INTEGER,          INTENT(INOUT) :: i_Kount2
REAL (KIND(0D0)), INTENT(OUT)   :: d_Delta1
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     SUBROUTINE ptmpxact
! DESCRIPTION:
!     Compute Exact PTMP.
! LANGUAGE:
!     Fortran 90/95
! AUTHOR:
!     Dr. Paul W. Mielke, Jr., Dept of Statistics, Colorado State University
!        http://www.stat.colostate.edu/~mielke/mielkesoftware.html
!     Modifications:
!        Jon D. Richards, Operations Research Analyst    jon_richards@usgs.gov
!        Fort Collins Science Center                  http://www.fort.usgs.gov
!        U.S. Geological Survey  -  Biological Resources Division
! INVOKED BY:
!     ptmpxact <File: "mrppmod.f90: ptmpxact"> module
!
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!___Procedure Parameters:
REAL (KIND(0D0)), PARAMETER :: d_PREC = 1.0D-12
!___Local Variables:
REAL (KIND(0D0)) :: d_Delta, d_Sum
INTEGER :: i, j
!___Intrinsic Procedures:
INTRINSIC :: ABS, DBLE
!___Executable Statements:
d_Sum = d_ZERO
DO i=1,i_NumPairs-1
   DO j=i+1,i_NumPairs
      d_Sum = d_Sum + (ABS( da_X(i)*DBLE(ia_M(i)) - da_X(j)*DBLE(ia_M(j)) ))**d_V
      call rchkusr()
   END DO
END DO
d_Delta = d_Sum*d_TWO/(d_FN*(d_FN-d_ONE))
IF (i_Kount1 == 1) d_Delta1 = d_Delta*(d_ONE+d_PREC)
IF (d_Delta <= d_Delta1) i_Kount2 = i_Kount2 + 1
RETURN   ! ... normal exit ...
END SUBROUTINE ptmpxact

!=============================================================================!

SUBROUTINE sptmp(i_NumPerms, i_NumPairs, d_V, da_D1, da_D2, d_Delta1,         &
      d_PValue, iEr,l_SaveTest,da_STV,i_seed)

USE blcmnmod, ONLY: d_HALF, d_ONE, d_TWO, d_ZERO, i_OK, i_ZERO
        ! ^-- Some global Blossom variables and parameters.
USE jonsmodule, ONLY: i_CCode, i_MRPPMOD_ER024
        ! ^-- Message handling, etc.
!___Imported Procedures:
USE jonsmodule, ONLY: lf_Equals
        ! ^-- lf_Equals: Generic test for equality of two objects.
USE mt19937, ONLY: genrand_real3
        ! ^-- Mersenne Twister Pseudo Random Number Generator
IMPLICIT NONE
INTEGER, PARAMETER :: i_SAVETEST_UNIT = 89 ! File Unit
CHARACTER (LEN=260) :: ch_SaveTestFile ! current Blossom SAVETEST file.
CHARACTER (LEN=260) :: ch_UseFile  ! current Blossom USE file.CHARACTER (LEN=MAX_PATH) :: ch_UseFile  ! current Blossom USE file.
CHARACTER (LEN=260) :: ch_OutPath     = " "
CHARACTER (LEN=260) :: ch_UsePath     = " " ! added August 2007 - jdr
!___Dummy Arguments:
INTEGER,          INTENT(IN)  :: i_NumPerms
INTEGER,          INTENT(IN)  :: i_NumPairs
REAL (KIND(0D0)), INTENT(IN)  :: d_V
REAL (KIND(0D0)), INTENT(IN)  :: da_D1(:)
REAL (KIND(0D0)), INTENT(IN)  :: da_D2(:)
REAL (KIND(0D0)), INTENT(OUT) :: d_Delta1
REAL (KIND(0D0)), INTENT(OUT) :: d_PValue
! INTEGER,        INTENT(OUT) :: i_NumNonZeroDiff
INTEGER,          INTENT(OUT) :: iEr
REAL (KIND(0D0)), INTENT(OUT) :: da_STV(:)  ! save test values
INTEGER,          INTENT(INOUT)    :: i_seed
LOGICAL,  		   INTENT (IN)   :: l_SaveTest
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     SUBROUTINE sptmp
! DESCRIPTION
!         PERMUTATION TEST FOR MATCHED PAIRS (SPTMP) YIELDS AN APPROXIMATE
!         P-VALUE (POWER OF RANKS TEST IS INCLUDED AS A SPECIAL CASE).
!         AN OPTION OF THIS PROGRAM IS THE ONE-SAMPLE TEST.
!         THIS PROGRAM IS CAPABLE OF MULTIPLE RUNS.
!         PROGRAM MODIFIED 11/19/99
!
!      i_NumPerms  Number of simulations
!      i_NumPairs  Number of original pairs
!      d_V         Distance exponent
!      da_D1       Array of 1st of the data pairs
!      da_D2       Array of 2nd of the data pairs
!      d_Delta1    Observed Delta
!      d_PValue    Simulated P-Value (approximate)
!                          P-Value of a smaller or equal delta
!      i_NumNonZeroDiff  Number of non-zero values or differences
!
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!___Procedure Parameters:
REAL (KIND(0D0)), PARAMETER :: d_PREC = 1.0D-12
!___Local Variables:
REAL (KIND(0D0)), ALLOCATABLE :: da_X(:)   ! Array of Differences.
REAL (KIND(0D0)) :: d_Diff, d_FN, d_Sum, d_SX, d_T
! REAL (KIND(0D0)) :: da_U(17)
INTEGER :: i, IS, j
INTEGER :: ios
INTEGER :: i_MP
!___Intrinsic Procedures:
INTRINSIC :: ABS, DBLE, RANDOM_NUMBER

iEr = i_OK ! no problem yet
d_Delta1 = d_ZERO
d_PValue = d_ZERO

ALLOCATE(da_X(1:i_NumPairs), STAT=ios)       ! compiler says set but never used
IF (ios /= i_OK) THEN
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |_______
!  |                                          |
!  | 'Memory allocation error: da_X in SPTMP' |
!  |__________________________________________|
!
   i_CCode = ios
   iEr = i_MRPPMOD_ER024
   RETURN   ! ... error exit ...
END IF

da_X = da_D1 - da_D2
d_Sum = d_ZERO

DO i=1,i_NumPairs-1
   DO j=i+1,i_NumPairs
      d_Sum = d_Sum + (ABS(da_X(i)-da_X(j)))**d_V
      call rchkusr()
   END DO
END DO
d_Sum = d_Sum*(d_ONE+d_PREC)  ! original
IF (l_SAVETEST) THEN ! We want to save original and all resampled test statistics
   da_STV(1) = d_Sum
END IF

i_MP  = 1

DO IS=1,i_NumPerms-1          ! do the resampling

   DO i=1,i_NumPairs
      ! The following is a call to UNI, a uniform random number
      ! generator (0,1) for which we will use the Fortran 90/95
      ! intrinsic procedure RANDOM_NUMBER:

         d_T = genrand_real3( )
      IF (d_T < d_HALF) da_X(i) = d_ZERO - da_X(i)
      call rchkusr()
      !
   END DO
   d_SX = d_ZERO

   DO i=1,i_NumPairs-1
      DO j=i+1,i_NumPairs
         d_SX = d_SX + (ABS(da_X(i)-da_X(j)))**d_V
         call rchkusr()
      END DO
   END DO
   IF (l_SAVETEST) THEN  ! Save value of test stat for this permuted sample
      da_STV(IS+1) = d_SX
   END IF
   IF (d_SX < d_Sum) i_MP = i_MP + 1   ! compare resampled vs original
   call rchkusr()
END DO

d_PValue = DBLE(i_MP)/DBLE(i_NumPerms)

d_FN     = DBLE(i_NumPairs)
d_Delta1 = d_Sum*d_TWO/(d_FN*(d_FN-d_ONE))
DEALLOCATE(da_X, STAT=ios)

! /////////////////////////////////////////////////////////////////////////////////////////////
!
! Check to see that we have access to SAVETEST file
!
! /////////////////////////////////////////////////////////////////////////////////////////////

RETURN
END SUBROUTINE sptmp

!=============================================================================!

SUBROUTINE mrbp(d_V, i_NumGrps, i_NumBlks, i_NumVars, i_NumPerms,             &
      l_DoResample, da_Data, d_Delta, d_ExpecDelta, d_Var, d_Gam, d_Rho, d_T, &
      d_PValue, iEr,l_SaveTest,da_STV)
!___Imported Parameters and Variables:
USE blcmnmod, ONLY: d_ZERO, i_OK
        ! ^-- Some global Blossom variables and parameters.
!___Imported Procedures:
!__(none)__
IMPLICIT NONE
!___Dummy Arguments:
REAL (KIND(0D0)), INTENT(IN)    :: d_V            ! distance exponent
INTEGER,          INTENT(IN)    :: i_NumGrps      ! number of groups
INTEGER,          INTENT(IN)    :: i_NumBlks      ! number of blocks
INTEGER,          INTENT(IN)    :: i_NumVars      ! # resp vars
INTEGER,          INTENT(IN)    :: i_NumPerms     ! # resamples
LOGICAL,          INTENT(IN)    :: l_DoResample   ! resample?
LOGICAL,          INTENT(IN)    :: l_SaveTest
REAL (KIND(0D0)), INTENT(INOUT) :: da_Data(:,:,:) ! data
REAL (KIND(0D0)), INTENT(OUT)   :: d_Delta        ! Delta(D)
REAL (KIND(0D0)), INTENT(OUT)   :: d_ExpecDelta   ! Expected Delta
REAL (KIND(0D0)), INTENT(OUT)   :: d_Var          ! Variance of Delta
REAL (KIND(0D0)), INTENT(OUT)   :: d_Gam          ! Skewness of Delta
REAL (KIND(0D0)), INTENT(OUT)   :: d_Rho          ! Agreement Measure
REAL (KIND(0D0)), INTENT(OUT)   :: d_T            ! (D-E[D])/STDEV(D)
REAL (KIND(0D0)), INTENT(OUT)   :: d_PValue       ! P-value
INTEGER,          INTENT(OUT)   :: iEr            ! return error code
REAL (KIND(0D0)), INTENT(OUT) :: da_STV(:)  ! save test values

! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !

iEr = i_OK ! no problem yet

   CALL mrbpcalc(d_V, i_NumGrps, i_NumBlks, i_NumVars, i_NumPerms, &
         l_DoResample, da_Data, d_Delta, d_ExpecDelta, d_Var, d_Gam, d_Rho, &
         d_T, d_PValue, iEr,l_SaveTest,da_STV)



RETURN   ! ... normal exit ...
END SUBROUTINE mrbp

!==============================================================================

SUBROUTINE mrbpcalc(d_V, i_NumGrps, i_NumBlks, i_NumVars, i_NumPerms,         &
      l_DoResample, da_Data, d_Delta, d_ExpecDelta, d_Var, d_Gam, d_Rho, d_T, &
      d_PValue, iEr,l_SaveTest,da_STV)
!___Imported Parameters and Variables:
!USE bfilemod, ONLY: l_SAVETEST, i_SAVETEST_UNIT, ch_SaveTestFile, ch_UseFile, &
!      ch_OutPath, ch_UsePath
        ! ^-- Misc. Blossom file names, units, status parameters.
USE blcmnmod, ONLY: d_HALF, d_ONE, d_TWO, d_ZERO, i_OK
        ! ^-- Some global Blossom variables and parameters.
USE jonsmodule, ONLY: i_CCode, i_MRPPMOD_ER010, i_MRPPMOD_ER011, i_MRPPMOD_ER025
        ! ^-- Message handling, etc.
!___Imported Procedures:
USE jonsmodule, ONLY: lf_Equals
        ! ^-- lf_Equals: Generic test for equality of two objects.
USE pv_modul, ONLY: pvalue
        ! ^-- pvalue: compute P-value.
IMPLICIT NONE

!___Dummy Arguments:
REAL (KIND(0D0)), INTENT(IN)    :: d_V
INTEGER,          INTENT(IN)    :: i_NumGrps
INTEGER,          INTENT(IN)    :: i_NumBlks
INTEGER,          INTENT(IN)    :: i_NumVars
INTEGER,          INTENT(IN)    :: i_NumPerms   ! # resamples
LOGICAL,          INTENT(IN)    :: l_DoResample ! resample?
LOGICAL,          INTENT(IN)    :: l_SaveTest
REAL (KIND(0D0)), INTENT(INOUT) :: da_Data(:,:,:)
REAL (KIND(0D0)), INTENT(OUT)   :: d_Delta
REAL (KIND(0D0)), INTENT(OUT)   :: d_ExpecDelta
REAL (KIND(0D0)), INTENT(OUT)   :: d_Var
REAL (KIND(0D0)), INTENT(OUT)   :: d_Gam
REAL (KIND(0D0)), INTENT(OUT)   :: d_Rho
REAL (KIND(0D0)), INTENT(OUT)   :: d_T
REAL (KIND(0D0)), INTENT(OUT)   :: d_PValue
INTEGER,          INTENT(OUT)   :: iEr
REAL (KIND(0D0)), INTENT(OUT) :: da_STV(:)  ! save test values
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     SUBROUTINE mrbpcalc
! DESCRIPTION:
!     Calculations for MRBP. Uses compact 1-D distance matrix.
! LANGUAGE:
!     Fortran 90/95
! AUTHOR:
!     Dr. Paul W. Mielke, Jr., Dept of Statistics, Colorado State University
!        http://www.stat.colostate.edu/~mielke/mielkesoftware.html
!     Modifications:
!        Jon D. Richards, Operations Research Analyst    jon_richards@usgs.gov
!        Fort Collins Science Center                  http://www.fort.usgs.gov
!        U.S. Geological Survey  -  Biological Resources Division
! INVOKED BY:
!     mrbp      <File: "mrppmod.f90: mrbp">         module
! INVOKES:
!     lf_Equals <File: "jonsmodule.f90: lf_Equals"> module
!     pvalue    <File: "pv_modul.f90: pvalue">      module
!     sampwor   <File: "mrppmod.f90: sampwor">      module
! FILES:
!     -
! DEVICES:

! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!___Procedure Parameters:
REAL (KIND(0D0)), PARAMETER :: d_PREC = 1.0D-12
!___Local Variables:
REAL (KIND(0D0)), ALLOCATABLE :: da_SJ(:,:,:)
REAL (KIND(0D0)), ALLOCATABLE :: da_SJ2(:,:,:)
REAL (KIND(0D0)), ALLOCATABLE :: da_SJ3(:,:,:)
REAL (KIND(0D0)), ALLOCATABLE :: da_Dis(:)  ! to vector
REAL (KIND(0D0)), ALLOCATABLE :: da_DT(:,:)
REAL (KIND(0D0)), ALLOCATABLE :: da_SIJ(:,:)
REAL (KIND(0D0)), ALLOCATABLE :: da_SIJ2(:,:)
REAL (KIND(0D0)), ALLOCATABLE :: da_SIJ3(:,:)
REAL (KIND(0D0)), ALLOCATABLE :: da_TIJ2(:,:)
REAL (KIND(0D0)), ALLOCATABLE :: da_TIJ3(:,:)
REAL (KIND(0D0)), ALLOCATABLE :: da_UIJ(:,:)
REAL (KIND(0D0)), ALLOCATABLE :: da_VI(:,:)
REAL (KIND(0D0)) :: BC2, C0, C1, C2, C3, T1, T2, UJ
REAL (KIND(0D0)) :: d_DX, d_DZ
REAL (KIND(0D0)) :: d_Sum
REAL (KIND(0D0)) :: dtmp
REAL (KIND(0D0)) :: WI1, WI2, WI3, YIJ, ZIJK  ! Simplified. JDR
INTEGER :: i, j, k, l, IR, IS, IS1, IT, IT1, LO, IJ, KL, m
INTEGER :: IRR, ISS, JSS, JTT, KTT
INTEGER :: ios
INTEGER :: i_MP, i_IW
INTEGER :: i_GXB, ix1, ix2, ix3, ip1, jp1, ip2, jp2, ip3, jp3
!___Intrinsic Procedures:
INTRINSIC :: DBLE, HUGE, SQRT
!___Statement Function:
INTEGER dispos

dispos(i, j) = i + (2*i_NumGrps*i_NumBlks-j)*(j-1)/2
iEr = i_OK ! no problem yet
i_GXB = i_NumGrps*i_NumBlks


ALLOCATE(da_Dis(1:i_GXB + i_GXB*(i_GXB-1)/2), STAT=ios)
IF (ios /= i_OK) THEN
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |____________
!  |                                               |
!  | 'Memory allocation error: da_Dis in MRBPCALC' |
!  |_______________________________________________|
!
   i_CCode = ios
   iEr = i_MRPPMOD_ER010

   RETURN   ! ... error exit ...

END IF



BC2 = DBLE(i_NumBlks*(i_NumBlks-1))/d_TWO  ! A "handy" abbreviated expression.

DO i=1,i_NumGrps          ! Now, compute the distances
   DO j=1,i_NumBlks
      DO k=i,i_NumGrps
         LO = 1
         IF (i == k) LO = j
         DO l=LO,i_NumBlks
            IJ = i_NumBlks*(i-1) + j
            KL = i_NumBlks*(k-1) + l
            d_Sum = d_ZERO
            DO m=1,i_NumVars
               d_Sum = d_Sum + (da_Data(i,j,m)-da_Data(k,l,m))**2
              call rchkusr()
            END DO
            IF (IJ <= KL) THEN
               ip1 = IJ
               jp1 = KL
            ELSE
               ip1 = KL
               jp1 = IJ
            END IF

            ix1 = dispos(jp1, ip1)

            da_Dis(ix1) = d_Sum**(d_V/d_TWO)
           call rchkusr()
         END DO
      END DO
   END DO
END DO


IF (l_DoResample) THEN
        ! initialize our random number generator.
        ! The following code segment is Mielke's Random Number Generator
        ! initialization.  We will use Fortran 90's built-in procedures
        ! because they are considerably faster and at least comparably good.

   d_Delta = d_ZERO     ! Calculate the "Delta" for the original data order.
   DO IS=2,i_NumBlks
      IS1 = IS - 1
      DO IR=1,IS1
         DO i=1,i_NumGrps
            IRR = (i-1)*i_NumBlks + IR
            ISS = (i-1)*i_NumBlks + IS

            IF (ISS > IRR) THEN
               ip1 = IRR
               jp1 = ISS
            ELSE
               ip1 = ISS
               jp1 = IRR
            END IF

            ix1 = dispos(jp1, ip1)
            d_Delta = d_Delta + da_Dis(ix1)
            call rchkusr()
         END DO
      END DO
   END DO

   ALLOCATE(da_DT(1:i_NumGrps,1:i_NumVars), STAT=ios)
   IF (ios /= i_OK) THEN
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |_______
!  |                                          |
!  | 'Memory allocation error: da_DT in MRBP' |
!  |__________________________________________|
!
      i_CCode = ios
      DEALLOCATE(da_Dis, STAT=ios)
      iEr = i_MRPPMOD_ER025
      RETURN   ! ... error exit ...
   END IF
   C0 = BC2*DBLE(i_NumGrps) ! Another "handy" abbreviated expression.

   d_Delta = d_Delta/C0

   IF (l_SAVETEST) THEN ! We want to save original and all resampled test statistics
      da_STV(1) = d_Delta
   END IF
   d_DX = d_Delta*(d_ONE + d_PREC) ! Observed Delta + "skosh" to test against.

   i_MP = 1  ! Delta satisfies criterion for accum. stat.   JDR   4 May 2000




   DO i_IW=1,i_NumPerms-1  ! 1(above) + these gives i_NumPerms  JDR   4 May 2000
      DO j=2,i_NumBlks    ! Loop to resample from the data.
         DO i=1,i_NumGrps
            da_DT(i,1:i_NumVars) = da_Data(i,j,1:i_NumVars)
            call rchkusr()
         END DO
        ! CALL SAMPWOR(i_NumGrps, i_NumGrps, i_NumVars, da_U, da_DT)
         CALL SAMPWOR(i_NumGrps, i_NumGrps, i_NumVars, da_DT)
         DO i=1,i_NumGrps
            da_Data(i,j,1:i_NumVars) = da_DT(i,1:i_NumVars)
         END DO
      END DO
      da_Dis = d_ZERO ! Array initialization
      DO i=1,i_NumGrps        ! Find distances for the Resampled data.
         DO j=1,i_NumBlks
            DO k=i,i_NumGrps
               LO = 1
               IF (i == k) LO = j
               DO l=LO,i_NumBlks
                  IJ = i_NumBlks*(i-1) + j
                  KL = i_NumBlks*(k-1) + l
                  d_Sum = d_ZERO
                  DO m=1,i_NumVars
                     d_Sum = d_Sum + (da_Data(i,j,m)-da_Data(k,l,m))**2
                     call rchkusr()
                  END DO
                  IF (KL <= IJ) THEN
                     ip1 = IJ
                     jp1 = KL
                  ELSE
                     ip1 = KL
                     jp1 = IJ
                  END IF
                  ix1 = dispos(ip1, jp1)
                  da_Dis(ix1) = d_Sum**(d_V/d_TWO)
                  call rchkusr()
               END DO
            END DO
         END DO
      END DO
      d_DZ=d_ZERO        ! Calculate "Delta" for Resampled data.
      DO IS=2,i_NumBlks
         IS1 = IS - 1
         DO IR=1,IS1
            DO i=1,i_NumGrps
               IRR = (i-1)*i_NumBlks + IR
               ISS = (i-1)*i_NumBlks + IS
               IF (ISS <= IRR) THEN
                  ip1 = IRR
                  jp1 = ISS
               ELSE
                  ip1 = ISS
                  jp1 = IRR
               END IF
               ix1 = dispos(ip1, jp1)
               d_DZ = d_DZ + da_Dis(ix1)
               call rchkusr()
            END DO
         END DO
      END DO
      d_DZ = d_DZ/C0
      IF (d_DZ < d_DX) i_MP = i_MP + 1  ! Test resampled Delta vs. original Delta.
      IF (l_SAVETEST) THEN  ! Save value of test stat for this permuted sample
         da_STV(i_IW+1) = d_DZ
      END IF
   END DO


   DEALLOCATE(da_DT, da_Dis, STAT=ios)
   d_PValue = DBLE(i_MP)/DBLE(i_NumPerms) ! P-Value is this simple proportion.
        ! END RESAMPLING to get P-Value

! /////////////////////////////////////////////////////////////////////////////////////////////
!
! Check to see that we have access to SAVETEST file
!
! /////////////////////////////////////////////////////////////////////////////////////////////


ELSE


        ! BEGIN PEARSON TYPE III APPROXIMATION to get P-Value.

   ALLOCATE(                                        &
        da_SJ(1:i_NumGrps,1:i_NumBlks,1:i_NumBlks), &
       da_SJ2(1:i_NumGrps,1:i_NumBlks,1:i_NumBlks), &
       da_SJ3(1:i_NumGrps,1:i_NumBlks,1:i_NumBlks), &
       da_SIJ(1:i_NumBlks,1:i_NumBlks),             &
      da_SIJ2(1:i_NumBlks,1:i_NumBlks),             &
      da_SIJ3(1:i_NumBlks,1:i_NumBlks),             &
       da_UIJ(1:i_NumBlks,1:i_NumBlks),             &
      da_TIJ2(1:i_NumBlks,1:i_NumBlks),             &
      da_TIJ3(1:i_NumBlks,1:i_NumBlks),             &
        da_VI(1:i_NumBlks,1:i_NumBlks),             &
		STAT=ios)
   IF (ios /= i_OK) THEN
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |_________________________
!  |                                                            |
!  | 'Memory allocation error: da_SJ,da_SJ2,da_SJ3,da_SIJ,      |
!  |  da_SIJ2,da_SIJ3,da_UIJ,da_TIJ2,da_TIJ3,da_VI in MRBPCALC' |
!  |____________________________________________________________|
!
      DEALLOCATE(da_Dis, STAT=ios)
      i_CCode = iEr
      iEr = i_MRPPMOD_ER011

      RETURN   ! ... error exit ...
   END IF

   DO IS=1,i_NumBlks

      DO IR=1,i_NumBlks

         IF (IR /= IS) THEN
            da_SIJ(IR,IS)  = d_ZERO
            da_SIJ2(IR,IS) = d_ZERO
            da_SIJ3(IR,IS) = d_ZERO
            DO i=1,i_NumGrps
               da_SJ(i,IR,IS)  = d_ZERO
               da_SJ2(i,IR,IS) = d_ZERO
               da_SJ3(i,IR,IS) = d_ZERO
               DO j=1,i_NumGrps
                  IRR = (i-1)*i_NumBlks + IR
                  JSS = (j-1)*i_NumBlks + IS
                  IF (JSS <= IRR) THEN
                     ip1 = IRR
                     jp1 = JSS
                  ELSE
                     ip1 = JSS
                     jp1 = IRR
                  END IF
                  ix1 = dispos(ip1, jp1)
                  da_SJ(i,IR,IS)  = da_SJ(i,IR,IS)  + da_Dis(ix1)
                  da_SJ2(i,IR,IS) = da_SJ2(i,IR,IS) + da_Dis(ix1)**2
                  da_SJ3(i,IR,IS) = da_SJ3(i,IR,IS) + da_Dis(ix1)**3
               call rchkusr()
               END DO
               da_SIJ(IR,IS)  = da_SIJ(IR,IS)  + da_SJ(i,IR,IS)
               da_SIJ2(IR,IS) = da_SIJ2(IR,IS) + da_SJ2(i,IR,IS)
               da_SIJ3(IR,IS) = da_SIJ3(IR,IS) + da_SJ3(i,IR,IS)
            END DO
         END IF
      END DO
   END DO

   T2 = d_ZERO
   IF (i_NumBlks > 2) THEN

      DO IT=3,i_NumBlks

         IT1 = IT - 1
         DO IS=2,IT1

            IS1 = IS - 1
            DO IR=1,IS1
               WI1  = d_ZERO  ! Simplified. JDR
               WI2  = d_ZERO  ! Simplified. JDR
               WI3  = d_ZERO  ! Simplified. JDR
               YIJ  = d_ZERO  ! Simplified. JDR
               ZIJK = d_ZERO  ! Simplified. JDR

               DO i=1,i_NumGrps
                  WI1 = WI1+da_SJ(i,IR,IS)*da_SJ(i,IR,IT) ! Simplified. JDR
                  WI2 = WI2+da_SJ(i,IS,IR)*da_SJ(i,IS,IT) ! Simplified. JDR
                  WI3 = WI3+da_SJ(i,IT,IR)*da_SJ(i,IT,IS) ! Simplified. JDR

                  DO j=1,i_NumGrps
                     IRR = i_NumBlks*(i-1)+IR
                     JSS = i_NumBlks*(j-1)+IS
                     JTT = i_NumBlks*(j-1)+IT
                     ISS = i_NumBlks*(i-1)+IS

                     IF (JSS <= IRR) THEN  ! (IRR,JSS)
                        ip1 = IRR
                        jp1 = JSS
                     ELSE
                        ip1 = JSS
                        jp1 = IRR
                     END IF
                     IF (JTT <= IRR) THEN  ! (IRR,JTT)
                        ip2 = IRR
                        jp2 = JTT
                     ELSE
                        ip2 = JTT
                        jp2 = IRR
                     END IF
                     IF (JTT <= ISS) THEN  ! (ISS,JTT)
                        ip3 = ISS
                        jp3 = JTT
                     ELSE
                        ip3 = JTT
                        jp3 = ISS
                     END IF
                     ix1 = dispos(ip1, jp1) !1
                     ix2 = dispos(ip2, jp2) !1
                     ix3 = dispos(ip3, jp3) !1

                     YIJ = YIJ + &
                           da_Dis(ix1)*da_SJ(i,IR,IT)*da_SJ(j,IS,IT) +    &
                           da_Dis(ix2)*da_SJ(i,IR,IS)*da_SJ(j,IT,IS) +    &
                           da_Dis(ix3)*da_SJ(i,IS,IR)*da_SJ(j,IT,IR)

                     DO k=1,i_NumGrps
                        KTT = (k-1)*i_NumBlks + IT

                        IF (JSS <= IRR) THEN   ! (IRR,JSS)
                           ip1 = IRR
                           jp1 = JSS
                        ELSE
                           ip1 = JSS
                           jp1 = IRR
                        END IF

                        IF (KTT <= IRR) THEN   ! (IRR,KTT)
                           ip2 = IRR
                           jp2 = KTT
                        ELSE
                           ip2 = KTT
                           jp2 = IRR
                        END IF

                        IF (KTT <= JSS) THEN   ! (JSS,KTT)
                           ip3 = JSS
                           jp3 = KTT
                        ELSE
                           ip3 = KTT
                           jp3 = JSS
                        END IF
                        ix1 = dispos(ip1, jp1) !1
                        ix2 = dispos(ip2, jp2) !1
                        ix3 = dispos(ip3, jp3) !1

                        ZIJK = ZIJK + da_Dis(ix1)* &
                                      da_Dis(ix2)* &
                                      da_Dis(ix3)
                     call rchkusr()
                     END DO
                  END DO
               END DO

               T2 = T2 + da_SIJ(IR,IS)*da_SIJ(IR,IT)*da_SIJ(IS,IT) -          &
                         da_SIJ(IS,IT)*WI1*i_NumGrps -                        &
                         da_SIJ(IR,IT)*WI2*i_NumGrps -                        &
                         da_SIJ(IR,IS)*WI3*i_NumGrps +                        &
                         YIJ*i_NumGrps*i_NumGrps -                            &
                         ZIJK*i_NumGrps*i_NumGrps*i_NumGrps

            END DO
         END DO
      END DO
      T2 = T2*6/(i_NumGrps-1)

   END IF

   DO IS=2,i_NumBlks

      IS1 = IS - 1
      DO IR=1,IS1

         da_TIJ2(IR,IS) = d_ZERO
         da_TIJ3(IR,IS) = d_ZERO
         da_VI(IR,IS)   = d_ZERO
         da_UIJ(IR,IS)  = d_ZERO
         DO i=1,i_NumGrps
            da_TIJ2(IR,IS) = da_TIJ2(IR,IS) + da_SJ(i,IR,IS)**2 + da_SJ(i,IS,IR)**2
            da_TIJ3(IR,IS) = da_TIJ3(IR,IS) + da_SJ(i,IR,IS)**3 + da_SJ(i,IS,IR)**3
            da_VI(IR,IS)   = da_VI(IR,IS)   +                                 &
                             da_SJ(i,IR,IS)*da_SJ2(i,IR,IS) +                 &
                             da_SJ(i,IS,IR)*da_SJ2(i,IS,IR)
            UJ = d_ZERO
            DO j=1,i_NumGrps
               IRR = i_NumBlks*(i-1) + IR
               JSS = i_NumBlks*(j-1) + IS

               IF (JSS <= IRR) THEN ! (IRR,JSS)
                  ip1 = IRR
                  jp1 = JSS
               ELSE
                  ip1 = JSS
                  jp1 = IRR
               END IF
               ix1 = dispos(ip1, jp1) ! ip+(2*i_NumObs-jp)*(jp-1)/2
               UJ  = UJ + da_Dis(ix1)*da_SJ(i,IR,IS)*da_SJ(j,IS,IR)
               call rchkusr()
            END DO
            da_UIJ(IR,IS) = da_UIJ(IR,IS) + UJ
         END DO
      END DO
   END DO

   T1          = d_ZERO
   d_ExpecDelta = d_ZERO
   d_Var        = d_ZERO

   DO IS=2,i_NumBlks

      IS1 = IS - 1
      DO IR=1,IS1

         IF (i_NumGrps > 2) THEN
            T1 = T1 + (da_SIJ(IR,IS)**3)*4.0D0 -                              &
                  da_SIJ(IR,IS)*da_TIJ2(IR,IS)*i_NumGrps*6.0D0 +              &
                  da_UIJ(IR,IS)*i_NumGrps*i_NumGrps*6.0D0 +                   &
                  da_TIJ3(IR,IS)*i_NumGrps*i_NumGrps*d_TWO +                  &
                  da_SIJ(IR,IS)*da_SIJ2(IR,IS)*i_NumGrps*i_NumGrps*3.0D0 -    &
                  da_VI(IR,IS)*i_NumGrps*i_NumGrps*i_NumGrps*3.0D0 +          &
                  da_SIJ3(IR,IS)*i_NumGrps*i_NumGrps*i_NumGrps*i_NumGrps
         END IF
         d_ExpecDelta = d_ExpecDelta + da_SIJ(IR,IS)
         d_Var        = d_Var + da_SIJ(IR,IS)*da_SIJ(IR,IS) -                 &
               da_TIJ2(IR,IS)*i_NumGrps+da_SIJ2(IR,IS)*i_NumGrps*i_NumGrps
               call rchkusr()
      END DO
   END DO
   IF (i_NumGrps > 2) T1 = T1/DBLE(i_NumGrps-2)
   C1   = d_ONE/(BC2*DBLE(i_NumGrps*i_NumGrps))
   C2   = C1*C1
   C3   = C2*C1
   d_ExpecDelta = C1*d_ExpecDelta
   d_Var        = d_Var*C2/DBLE(i_NumGrps-1)
           ! If variance is negative, set to d_ZERO.
   IF (d_Var < d_ZERO) d_Var = d_ZERO
           ! If variance if very small, set to d_ZERO.
   IF (d_Var < 1.0D-14) d_Var = d_ZERO
           ! If variance is d_ZERO, other stats are not defined set them to a
           ! very large number so that Blossom PROGRAM will be able to catch
           ! on, and have Blossom program explain on it's output.
   IF (lf_Equals(d_Var, d_ZERO)) THEN !* <File: "jonsmodule.f90: lf_Equals">
      d_Gam = HUGE(99.99D99)
   ELSE
      d_Gam = C3*(T1-T2)/DBLE(i_NumGrps-1)
      d_Gam = d_Gam/SQRT(d_Var**3)
   END IF

   d_Delta = d_ZERO

   DO IS=2,i_NumBlks

      IS1 = IS - 1
      DO IR=1,IS1

         DO i=1,i_NumGrps
            IRR = (i-1)*i_NumBlks + IR
            ISS = (i-1)*i_NumBlks + IS
            ! The next statement is to keep loop from too much optimization:
            ! IF (da_Dis(IRR,ISS) < 0.0D0) da_Dis(IRR,ISS) = ABS(da_Dis(IRR,ISS))
            ! d_Delta = d_Delta + da_Dis(IRR,ISS)
            IF (ISS <= IRR) THEN ! (IRR,ISS)
               ip1 = IRR
               jp1 = ISS
            ELSE
               ip1 = ISS
               jp1 = IRR
            END IF
            ix1 = dispos(ip1, jp1) ! ip+(2*i_NumGrps*i_NumBlks-jp)*(jp-1)/2
            !2 ix1 = dispos(jp1, ip1) ! ip+(2*i_NumGrps*i_NumBlks-jp)*(jp-1)/2
            ! The next statement is to keep loop from too much optimization:
            IF (da_Dis(ix1) < 0.0D0) da_Dis(ix1) = ABS(da_Dis(ix1))
            d_Delta = d_Delta + da_Dis(ix1)
            call rchkusr()
         END DO
      END DO
   END DO

   DEALLOCATE(da_Dis, da_SJ, da_SJ2, da_SJ3, da_SIJ, da_SIJ2, da_SIJ3,        &
         da_UIJ, da_TIJ2, da_TIJ3, da_VI, STAT=ios)
   C0 = BC2*DBLE(i_NumGrps)

   d_Delta = d_Delta/C0

   IF (lf_Equals(d_Var, d_ZERO)) THEN !* <File: "jonsmodule.f90: lf_Equals">
      d_T = HUGE(99.99D99)
   ELSE
      d_T = (d_Delta-d_ExpecDelta)/SQRT(d_Var)
   END IF
   d_Rho = d_ONE - (d_Delta/d_ExpecDelta)
   IF (lf_Equals(d_Var, d_ZERO)) THEN  ! if variance is ZERO:
      d_PValue = d_ONE          ! prob is 1.0, no need to try to call pvalue
   ELSE
      d_PValue = pvalue(d_T, d_Gam)   ! from pv_modul
           !* <File: "pv_modul.f90: pvalue">
   END IF
END IF  ! END PEARSON TYPE III Approximation to get P-Value
RETURN   ! ... normal exit ...

END SUBROUTINE mrbpcalc



!=============================================================================!

SUBROUTINE emrbp(d_V, i_NumGrps, i_NumBlks, i_NumVars, i_IA, i_IC, i_LR, d_H, &
      da_Data, i_KountA, d_Delta1, d_PValue, iEr)
!___Imported Parameters and Variables:
USE blcmnmod, ONLY: d_ONE, d_TWO, d_ZERO, i_OK, i_ZERO
        ! ^-- Some global Blossom variables and parameters.
USE ioformod, ONLY: ch12_OutBuff, ch12_OutBuff2
        ! ^-- Buffers for character output of numbers.
USE jonsmodule, ONLY: i_CCode, i_MRPPMOD_ER012, i_MRPPMOD_ER013
        ! ^-- Message handling, etc.
USE mrolamod, ONLY: i_MAX_EMRBP_BLOCKS, i_MIN_EMRBP_BLOCKS
        ! ^-- Some high-level Blossom parameters.
!___Imported Procedures:
USE jonsmodule, ONLY: lf_Equals
        ! ^-- lf_Equals: Generic test for equality of two objects.
IMPLICIT NONE
!___Dummy Arguments:
REAL (KIND(0D0)), INTENT(IN)    :: d_V
INTEGER,          INTENT(IN)    :: i_NumGrps
INTEGER,          INTENT(IN)    :: i_NumBlks
INTEGER,          INTENT(IN)    :: i_NumVars
INTEGER,          INTENT(IN)    :: i_IA     ! 1 for alignment
INTEGER,          INTENT(IN)    :: i_IC     ! 1 for median commens
INTEGER,          INTENT(IN)    :: i_LR     ! 1 for C(G,H) ranks test
REAL (KIND(0D0)), INTENT(IN)    :: d_H      ! ranks test exponent
REAL (KIND(0D0)), INTENT(INOUT) :: da_Data(:,:,:)
INTEGER,          INTENT(OUT)   :: i_KountA ! tot # permutations
REAL (KIND(0D0)), INTENT(OUT)   :: d_Delta1
REAL (KIND(0D0)), INTENT(OUT)   :: d_PValue
INTEGER,          INTENT(OUT)   :: iEr
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     SUBROUTINE emrbp
! DESCRIPTION:
!     Exact Multi-response Randomized Block Procedure
!     This program adapted from Fortran 77 programs written by
!     Dr. Paul Mielke at Colorado State University Statistics Department.
!         (from programs "emrppX.for", where X=2,...,9)
!     This Fortran program computes the test statistic and the exact
!     p-value for an analysis of a randomized block experiment with up
!     to 9 blocks (B = 9).
!     [OBSOLETE:The present maximum values of G and R are 10 and 15].
!     [OBSOLETE:As a consequence of
!           (3!)(3!)(3!)(3!)(3!)(3!)(3!)(3!) = 1,679,616,
!     G = 3 is likely at a maximum; however, R may be
!     increased if required].
!
!      This program is capable of performing alignment within blocks,
!      distance function commensuration, and C(G,H) ranks test.
!         .  .  .  .  .  .  .  .  .  .  .  .
!  Dummy Argument Description:
!                                     (IN) d_V       distance exponent
!                                     (IN) i_NumGrps number of groups
!                                     (IN) i_NumBlks number of blocks
!                                     (IN) i_NumVars number of variables
!                                     (IN) i_IA     1 implies alignment
!                                     (IN) i_IC     1 implies median commens.
!                                     (IN) i_LR     1 implies C(G,H) ranks test
!                                     (IN) d_H      ranks test exponent
!     dim(i_NumGrps,i_NumBlks,i_NumVars) (IN) da_Data  observation data
!                                    (OUT) i_KountA total # permutations
!                                    (OUT) d_Delta1 observed MRBP statistic
!                                    (OUT) d_PValue exact MRBP p-value
!                                    (OUT) iEr     error code (0=OK)

! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!___Procedure Parameters:
REAL (KIND(0D0)), PARAMETER :: d_PREC = 1.0D-12
INTEGER,          PARAMETER ::   i_ON = 1
INTEGER,          PARAMETER ::  i_OFF = 0
!___Local Variables:

REAL (KIND(0D0)), ALLOCATABLE :: da_AD(:)
        !                 ^ dim da_AD(#Resp=i_NumVars)
REAL (KIND(0D0)), ALLOCATABLE :: da_XM(:,:)
        !                 ^ dim da_XM(#Blks=i_NumBlks,#Resp=i_NumVars)
REAL (KIND(0D0)), ALLOCATABLE :: da_X(:,:,:)
        !   ^ dim da_X(#Grps=i_NumGrps,#Blks=i_NumBlks,#Resp=i_NumVars)
REAL (KIND(0D0)) :: d_A1, d_A2
REAL (KIND(0D0)) :: d_C
REAL (KIND(0D0)) :: d_Sum, d_Sum1, d_DM1, d_DM2
REAL (KIND(0D0)) :: d_SumS
REAL (KIND(0D0)) :: d_Sum_o
INTEGER, ALLOCATABLE :: ia_P2(:) ! dim(#Grps=i_NumGrps)
INTEGER, ALLOCATABLE :: ia_P3(:) ! dim(#Grps=i_NumGrps)
INTEGER, ALLOCATABLE :: ia_P4(:) ! dim(#Grps=i_NumGrps)
INTEGER, ALLOCATABLE :: ia_P5(:) ! dim(#Grps=i_NumGrps)
INTEGER, ALLOCATABLE :: ia_P6(:) ! dim(#Grps=i_NumGrps)
INTEGER, ALLOCATABLE :: ia_P7(:) ! dim(#Grps=i_NumGrps)
INTEGER, ALLOCATABLE :: ia_P8(:) ! dim(#Grps=i_NumGrps)
INTEGER, ALLOCATABLE :: ia_P9(:) ! dim(#Grps=i_NumGrps)
INTEGER :: i, j, k, I1, I2, J1, J2
INTEGER :: ios ! return status code
INTEGER :: i_IAUse, i_ICUse
INTEGER :: i_KountB ! count # times expected < observed MRBP statistic
!___Intrinsic Procedures:
INTRINSIC :: ABS, DBLE, SUM
!___Executable Statements:
iEr = i_OK ! no problem yet
i_IAUse = i_IA
i_ICUse = i_IC
ALLOCATE(                                         &
      da_AD(1:i_NumGrps),                         &
      da_XM(1:i_NumBlks,1:i_NumVars),             &
       da_X(1:i_NumGrps,1:i_NumBlks,1:i_NumVars), &
        !?x?       da_S(1:i_NumGrps,1:i_NumBlks,1:i_NumBlks),     &
      ia_P2(1:i_NumGrps),                         &
      ia_P3(1:i_NumGrps),                         &
      ia_P4(1:i_NumGrps),                         &
      ia_P5(1:i_NumGrps),                         &
      ia_P6(1:i_NumGrps),                         &
      ia_P7(1:i_NumGrps),                         &
      ia_P8(1:i_NumGrps),                         &
      ia_P9(1:i_NumGrps),                         &
      STAT =iEr)
IF (iEr /= i_OK) THEN
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |______________________
!  |                                                         |
!  | 'Memory allocation error: da_AD, da_XM,da_X,da_S,ia_P2, |
!  |  ia_P3,ia_P4,ia_P5,ia_P6,ia_P7,ia_P8,ia_P9 IN EMRBP'    |
!  |_________________________________________________________|
!
   DEALLOCATE(da_AD, da_XM, da_X, ia_P2, ia_P3, ia_P4, ia_P5, ia_P6, ia_P7,   &
         ia_P8, ia_P9, STAT=ios)
   i_CCode = ios
   iEr = i_MRPPMOD_ER012
   RETURN   ! ... error exit ...
END IF
da_AD = d_ZERO ! Initialize array.
da_XM = d_ZERO ! Initialize array.
da_X  = d_ZERO ! Initialize array.
        !?x? da_S = d_ZERO ! Initialize array.
ia_P2 = 0 ! Initialize array.
ia_P3 = 0 ! Initialize array.
ia_P4 = 0 ! Initialize array.
ia_P5 = 0 ! Initialize array.
ia_P6 = 0 ! Initialize array.
ia_P7 = 0 ! Initialize array.
ia_P8 = 0 ! Initialize array.
ia_P9 = 0 ! Initialize array.

IF (i_LR == i_ON) THEN                       ! BEGIN Ranks Test . . .
   i_IAUse = i_OFF  ! if ranks test, don't align
   i_ICUse = i_OFF  ! if ranks test, don't commensurate
   CALL emrbprank(i_NumGrps, & !* <File: "mrppmod.f90: emrbprank">
                  i_NumBlks, &
                  i_NumVars, &
                  d_H,       &
                  da_Data,   &
                  iEr)
   IF (iEr /= i_OK) THEN
      DEALLOCATE(da_AD, da_XM, da_X, ia_P2, ia_P3, ia_P4, ia_P5, ia_P6,       &
            ia_P7, ia_P8, ia_P9, STAT=ios)
      RETURN   ! ... error exit ...
   END IF
END IF
IF (i_IAUse == i_ON) THEN    ! ... alignment section:
   DO i=1,i_NumGrps
      DO j=1,i_NumBlks
         da_X(i,j,1:i_NumVars) = da_Data(i,j,1:i_NumVars)
      END DO
   END DO
   DO j=1,i_NumBlks
      DO k=1,i_NumVars
         d_A1   = 1.0D200
         d_Sum1 = d_A1
         DO I1=1,i_NumGrps
            d_Sum = SUM(ABS(da_Data(1:i_NumGrps,j,k)-da_X(I1,j,k)))
            IF (d_Sum < d_A1) THEN
               d_DM1 = da_X(I1,j,k)
               d_A1  = d_Sum
               d_A2  = d_A1*1.0000000001D0
            END IF
            IF (d_Sum < d_A2) THEN
               IF (.NOT.lf_Equals(d_DM1, da_X(I1,j,k))) THEN
                        !* <File: "jonsmodule.f90: lf_Equals">
                  d_DM2  = da_X(I1,j,k)
                  d_Sum1 = d_Sum
               END IF
            END IF
            call rchkusr()
         END DO
         IF (d_Sum1 > d_A2) d_DM2 = d_DM1
         da_XM(j,k) = (d_DM1+d_DM2)/d_TWO
         call rchkusr()
      END DO
   END DO
   DO i=1,i_NumGrps
      DO j=1,i_NumBlks
         DO k=1,i_NumVars
            da_Data(i,j,k) = da_Data(i,j,k) - da_XM(j,k)
            call rchkusr()
         END DO
      END DO
   END DO
END IF ! end alignment section
IF (i_ICUse == i_ON) THEN ! ... commensurations section with Cade's commens.:
   IF (i_NumVars > 1) THEN
      DO k=1,i_NumVars
         d_Sum_o = d_ZERO
         DO I1=1,i_NumGrps
            DO J1=1,i_NumBlks
               DO I2=1,i_NumGrps
                  DO J2=1,i_NumBlks
                     d_Sum_o = d_Sum_o + (ABS(da_Data(I1,J1,k)-da_Data(I2,J2,k)))**d_V
                     call rchkusr()
                  END DO
               END DO
            END DO
         END DO
         d_Sum_o = d_Sum_o / DBLE((i_NumBlks*i_NumGrps)*((i_NumBlks*i_NumGrps)-1))
         da_AD(k) = d_Sum_o**(d_ONE/d_V)
      END DO
      DO i=1,i_NumGrps
         DO j=1,i_NumBlks
            DO k=1,i_NumVars
               da_Data(i,j,k) = da_Data(i,j,k) / da_AD(k)
               call rchkusr()
            END DO
         END DO
      END DO
   END IF
END IF ! end commensuration section
d_Delta1 = d_ZERO
DO i=1,i_NumGrps
   DO j=1,i_NumBlks-1
      DO k=j+1,i_NumBlks

         d_SumS =  SUM((da_Data(i,j,1:i_NumVars)-da_Data(i,k,1:i_NumVars))**2)
         d_Delta1 = d_Delta1 + d_SumS**(d_V/d_TWO)
         call rchkusr()
      END DO
   END DO
END DO
d_C = d_TWO / DBLE(i_NumGrps*i_NumBlks*(i_NumBlks-1))
d_Delta1 = d_Delta1*(d_ONE+d_PREC)*d_C
CALL fact2(i_NumGrps, i_NumBlks, i_NumVars, i_KountA, i_KountB, d_Delta1,     &
      d_C, d_V, ia_P2, ia_P3, ia_P4, ia_P5, ia_P6, ia_P7, ia_P8, ia_P9,       &
      da_Data, iEr) !* <File: "mrppmod.f90: fact2">
IF (iEr /= i_OK) THEN
   DEALLOCATE(da_AD, da_XM, da_X, ia_P2, ia_P3, ia_P4, ia_P5, ia_P6, ia_P7,   &
         ia_P8, ia_P9, STAT=ios)
   RETURN   ! ... error exit ...
END IF
DEALLOCATE(da_AD, da_XM, da_X, ia_P2, ia_P3, ia_P4, ia_P5, ia_P6, ia_P7,      &
      ia_P8, ia_P9, STAT=ios)
d_PValue = DBLE(i_KountB)/DBLE(i_KountA)
RETURN   ! ... normal exit ...
END SUBROUTINE emrbp

!=============================================================================!

SUBROUTINE emrbprank(i_NumGrps, i_NumBlks, i_NumVars, d_H, da_Data, iEr)
!___Imported Parameters and Variables:
USE blcmnmod, ONLY: d_ONE, d_TWO, d_ZERO, i_OK
        ! ^-- Some global Blossom variables and parameters.
USE jonsmodule, ONLY: i_CCode, i_MRPPMOD_ER014
        ! ^-- Message handling, etc.
!___Imported Procedures:
!__(none)__
IMPLICIT NONE
!___Dummy Arguments:
INTEGER,          INTENT(IN)    :: i_NumGrps ! Number of Groups
INTEGER,          INTENT(IN)    :: i_NumBlks ! Number of Blocks
INTEGER,          INTENT(IN)    :: i_NumVars ! # Resp Vars
REAL (KIND(0D0)), INTENT(IN)    :: d_H ! emrbprank test exponent
        ! da_Data - observation data - dim(i_NumGrps,i_NumBlks,i_NumVars)
REAL (KIND(0D0)), INTENT(INOUT) :: da_Data(:,:,:)
INTEGER,          INTENT(OUT)   :: iEr ! error flag
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     SUBROUTINE emrbprank
! DESCRIPTION:
!     Computations for emrbp
! LANGUAGE:
!     Fortran 90/95
! AUTHOR:
!     Dr. Paul W. Mielke, Jr., Dept of Statistics, Colorado State University
!        http://www.stat.colostate.edu/~mielke/mielkesoftware.html
!     Modifications:
!        Jon D. Richards, Operations Research Analyst    jon_richards@usgs.gov
!        Fort Collins Science Center                  http://www.fort.usgs.gov
!        U.S. Geological Survey  -  Biological Resources Division

REAL (KIND(0D0)), ALLOCATABLE :: da_RKS(:) ! dim(#Grps=i_NumGrps)
REAL (KIND(0D0)) :: d_A1, d_A2, d_A3, d_A4, d_B1, d_B2
REAL (KIND(0D0)) :: d_CL, d_Phi, d_W, d_YM
INTEGER :: i, j, k
INTEGER :: ios
!___Intrinsic Procedures:
INTRINSIC :: ABS, DBLE, MINVAL
!___Executable Statements:
iEr = i_OK ! no problem yet
ALLOCATE(da_RKS(1:i_NumGrps), STAT=iEr)
IF (iEr /= i_OK) THEN
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |_____________
!  |                                                |
!  | 'Memory allocation error: da_RKS in EMRBPRANK' |
!  |________________________________________________|
!
   i_CCode = iEr
   iEr = i_MRPPMOD_ER014
   RETURN   ! ... error exit ...
END IF
da_RKS = d_ZERO ! Initialize array.
d_YM = DBLE(i_NumGrps+1)/d_TWO
DO j=1,i_NumBlks
   DO k=1,i_NumVars
      d_CL = MINVAL(da_Data(1:i_NumGrps,j,k))
      da_Data(1:i_NumGrps,j,k) = da_Data(1:i_NumGrps,j,k) - d_CL + d_ONE
      d_Phi = d_ONE + 1.0D-12
      d_A1  = d_ZERO
      d_A2  = d_ZERO
      d_A3  = DBLE(i_NumGrps) - 0.1D0
      d_B1  = d_ZERO - 1.0D30
      d_B2  = 1.0D30
      DO WHILE (d_A2<=d_A3)
         DO i=1,i_NumGrps
            IF (da_Data(i,j,k) > d_B1) THEN
               IF (da_Data(i,j,k) < d_B2) THEN
                  d_B2 = da_Data(i,j,k)*d_Phi
               END IF
            END IF
         END DO
         DO i=1,i_NumGrps
            d_W = ABS(d_ONE-da_Data(i,j,k)/d_B2)
            IF (d_W < 1.0D-11)  d_A1 = d_A1 + d_ONE
         END DO
         !          d_A4 = d_A2+(d_A1+1)/2
         d_A4 = d_A2+(d_A1+d_ONE)/d_TWO
         DO i=1,i_NumGrps
            d_W = ABS(d_ONE-da_Data(i,j,k)/d_B2)
            IF (d_W < 1.0D-11)  da_RKS(i) = d_A4
         END DO
         d_A2 = d_A2 + d_A1
         d_A1 = d_ZERO
         d_B1 = d_B2
         d_B2 = 1.0D30
      END DO  ! while(d_A2<=d_A3)
      DO i=1,i_NumGrps
         d_W = ABS(da_RKS(i)-d_YM)
         IF (d_W < 0.001D0)  THEN
            da_Data(i,j,k) = d_ZERO
         ELSE
            da_Data(i,j,k) = (da_RKS(i)-d_YM)*ABS(da_RKS(i)-d_YM)**(d_H-d_ONE)
         END IF
         call rchkusr()
      END DO
   END DO
END DO
DEALLOCATE(da_RKS, STAT=ios)
RETURN   ! ... normal exit ...
END SUBROUTINE emrbprank

!=============================================================================!

SUBROUTINE fact2(i_NumGrps, i_NumBlks, i_NumVars, i_KountA, i_KountB,         &
      d_Delta1, d_C, d_V, ia_P2, ia_P3, ia_P4, ia_P5, ia_P6, ia_P7, ia_P8,    &
      ia_P9, da_Data, iEr)
!___Imported Parameters and Variables:
USE blcmnmod, ONLY: i_OK
        ! ^-- Some global Blossom variables and parameters.
USE jonsmodule, ONLY: i_CCode, i_MRPPMOD_ER015
        ! ^-- Message handling, etc.
!___Imported Procedures:
!__(none)__
IMPLICIT NONE
!___Dummy Arguments:
INTEGER,          INTENT(IN)    :: i_NumGrps ! Number of Groups
INTEGER,          INTENT(IN)    :: i_NumBlks ! Number of Blocks
INTEGER,          INTENT(IN)    :: i_NumVars ! # Resp Vars
        ! i_KountA - count to total # permutations
INTEGER,          INTENT(OUT)   :: i_KountA
        ! i_KountB - count # times expected < observed MRBP statistic
INTEGER,          INTENT(OUT)   :: i_KountB
REAL (KIND(0D0)), INTENT(IN)    :: d_Delta1 ! Obs MRBP Stat
REAL (KIND(0D0)), INTENT(IN)    :: d_C
REAL (KIND(0D0)), INTENT(IN)    :: d_V ! distance exponent
INTEGER,          INTENT(INOUT) :: ia_P2(:) ! dim(i_NumGrps)
INTEGER,          INTENT(INOUT) :: ia_P3(:) ! dim(i_NumGrps)
INTEGER,          INTENT(INOUT) :: ia_P4(:) ! dim(i_NumGrps)
INTEGER,          INTENT(INOUT) :: ia_P5(:) ! dim(i_NumGrps)
INTEGER,          INTENT(INOUT) :: ia_P6(:) ! dim(i_NumGrps)
INTEGER,          INTENT(INOUT) :: ia_P7(:) ! dim(i_NumGrps)
INTEGER,          INTENT(INOUT) :: ia_P8(:) ! dim(i_NumGrps)
INTEGER,          INTENT(INOUT) :: ia_P9(:) ! dim(i_NumGrps)

REAL (KIND(0D0)), INTENT(INOUT) :: da_Data(:,:,:)
INTEGER,          INTENT(OUT)   :: iEr ! error flag
!. . . . . . . . . . . . . . . SUBROUTINE FACT2 . . . . . . . . . . . . . . . !

!. . . . . . . . . . . . . . . SUBROUTINE FACT2 . . . . . . . . . . . . . . . !
!___Local Variables:
INTEGER, ALLOCATABLE :: ia_A(:,:)   ! dim(i_NumGrps,i_NumGrps)
INTEGER :: i, j, k, l, m, ii, jj, ll
INTEGER :: ios
INTEGER :: i_Flag, i_Kount2, i_Limit, i_NMarks, i_Temp
!___Intrinsic Procedures:
!__(none)__
!. . . . . . . . . . . . . . . SUBROUTINE FACT2 . . . . . . . . . . . . . . . !
!___Executable Statements:
   iEr = i_OK ! no problem yet
   ALLOCATE(ia_A(1:i_NumGrps,1:i_NumGrps), STAT=iEr)
   IF (iEr /= i_OK) THEN
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |_______
!  |                                          |
!  | 'Memory allocation error: ia_A in FACT2' |
!  |__________________________________________|
!
      i_CCode = iEr
      iEr = i_MRPPMOD_ER015
      RETURN   ! ... error exit ...
   END IF
   ia_A = 0 ! Initialize array.
   k = i_NumGrps
   l = i_NumGrps
   DO i=1,k-1
      ia_A(i,1) = i
      DO j=2,l
         ia_A(i,j) = i + 1
      END DO
      l = l - 1
   END DO
   i_Kount2 = 0
   i_KountA = 0
   i_KountB = 0
   ! 10    CONTINUE
   loop10: DO
      i = k - 1
      i_NMarks = 2
      IF (i_NumBlks == 2) THEN
         IF (i_KountA == 0) GOTO 40
      ELSE
         IF (i_Kount2 == 0) GOTO 40
      END IF
      !    20    CONTINUE
      loop20: DO
         DO j=1,i_NMarks-1
            IF (ia_A(i,j) == i) THEN
               IF (ia_A(i,j+1) == i+1) THEN
                  i_Limit = j - 2
                  ia_A(i,j) = ia_A(i,j+1)
                  ia_A(i,j+1) = ia_A(i,j) - 1
                  IF (i_Limit <= 0) GOTO 40
                  !                   GOTO 30
                  EXIT loop20
               END IF
            END IF
            call rchkusr()
         END DO
         IF (i_NMarks /= 1) THEN
            DO j=1,i_NMarks/2
               i_Temp = ia_A(i,j)
               ia_A(i,j) = ia_A(i,i_NMarks-j+1)
               ia_A(i,i_NMarks-j+1) = i_Temp
               call rchkusr()
            END DO
         END IF
         i = i - 1
         IF (i == 0) THEN
            DEALLOCATE(ia_A, STAT=ios)
            RETURN                        ! ... normal exit ...
         END IF
         i_NMarks = i_NMarks + 1
         !          GOTO 20
      END DO loop20
        !     30   CONTINUE
       i_Flag = 1
       DO WHILE (i_Flag==1)
          i_Flag = 0
          DO j=1,i_Limit
             IF (ia_A(i,j) == i+1) THEN
               IF (ia_A(i,j+1) == i) THEN
                   ia_A(i,j) = ia_A(i,j+1)
                   ia_A(i,j+1) = ia_A(i,j) + 1
                   i_Flag = 1
                END IF
             END IF
             call rchkusr()
          END DO
       END DO

40    CONTINUE
      ia_A(k,1:i_NumGrps) = ia_A(1,1:i_NumGrps)
      IF (k /= 2) THEN
         DO l=2,k-1
            m = 1
            DO j=1,i_NumGrps
               IF (ia_A(k,j) == l) THEN
                  ia_A(k,j) = ia_A(l,m)
                  m = m + 1
               END IF
               call rchkusr()
            END DO
         END DO
      END IF
      ll = 1
      DO ii=1,k
         DO jj=1,i_NumGrps
            IF (ia_A(k,jj) == ii) THEN
               ia_P2(ll) = jj
               ll = ll + 1
            END IF
            call rchkusr()
         END DO
      END DO
      IF (i_NumBlks == 2) THEN
         CALL emrbpdval(i_NumGrps, i_NumBlks, i_NumVars, i_KountB, d_Delta1,  &
               d_C, d_V, ia_P2, ia_P3, ia_P4, ia_P5, ia_P6, ia_P7, ia_P8,     &
               ia_P9, da_Data, iEr) !* <File: "mrppmod.f90: emrbpdval">
         IF (iEr /= i_OK) THEN
            DEALLOCATE(ia_A, STAT=ios)
            RETURN   ! ... error exit ...
         END IF
         i_KountA = i_KountA + 1
      ELSE ! i_NumBlks is > 2, GOTO next FACT routine
         CALL fact3(i_NumGrps, i_NumBlks, i_NumVars, i_KountA, i_KountB,      &
               d_Delta1, d_C, d_V, ia_P2, ia_P3, ia_P4, ia_P5, ia_P6, ia_P7,  &
               ia_P8, ia_P9, da_Data, iEr) !* <File: "mrppmod.f90: fact3">
         IF (iEr /= i_OK) THEN
            DEALLOCATE(ia_A, STAT=ios)
            RETURN   ! ... error exit ...
         END IF
         i_Kount2 = i_Kount2 + 1
      END IF
      !       GOTO 10
   END DO loop10
END SUBROUTINE fact2

!=============================================================================!

SUBROUTINE fact3(i_NumGrps, i_NumBlks, i_NumVars, i_KountA, i_KountB,         &
      d_Delta1, d_C, d_V, ia_P2, ia_P3, ia_P4, ia_P5, ia_P6, ia_P7, ia_P8,    &
      ia_P9, da_Data, iEr)
!___Imported Parameters and Variables:
USE blcmnmod, ONLY: i_OK
        ! ^-- Some global Blossom variables and parameters.
USE jonsmodule, ONLY: i_CCode, i_MRPPMOD_ER016
        ! ^-- Message handling, etc.
!___Imported Procedures:
!__(none)__
IMPLICIT NONE
!___Dummy Arguments:
INTEGER,           INTENT(IN)   :: i_NumGrps ! # of Groups
INTEGER,           INTENT(IN)   :: i_NumBlks ! # of Blocks
INTEGER,           INTENT(IN)   :: i_NumVars ! # Resp. Vars
        ! i_KountA - count to total # permutations
INTEGER,          INTENT(INOUT) :: i_KountA
        ! i_KountB - count # times expected < observed MRBP statistic
INTEGER,          INTENT(INOUT) :: i_KountB
REAL (KIND(0D0)), INTENT(IN)    :: d_Delta1 ! Obs MRBP Stat
REAL (KIND(0D0)), INTENT(IN)    :: d_C
REAL (KIND(0D0)), INTENT(IN)    :: d_V      ! distance exponent
INTEGER,          INTENT(INOUT) :: ia_P2(:) ! dim(i_NumGrps)
INTEGER,          INTENT(INOUT) :: ia_P3(:) ! dim(i_NumGrps)
INTEGER,          INTENT(INOUT) :: ia_P4(:) ! dim(i_NumGrps)
INTEGER,          INTENT(INOUT) :: ia_P5(:) ! dim(i_NumGrps)
INTEGER,          INTENT(INOUT) :: ia_P6(:) ! dim(i_NumGrps)
INTEGER,          INTENT(INOUT) :: ia_P7(:) ! dim(i_NumGrps)
INTEGER,          INTENT(INOUT) :: ia_P8(:) ! dim(i_NumGrps)
INTEGER,          INTENT(INOUT) :: ia_P9(:) ! dim(i_NumGrps)
  ! da_Data - observation data - dim(i_NumGrps,i_NumBlks,i_NumVars)
REAL (KIND(0D0)), INTENT(INOUT) :: da_Data(:,:,:)
INTEGER,          INTENT(OUT)   :: iEr      ! error flag
!. . . . . . . . . . . . . . . SUBROUTINE FACT3 . . . . . . . . . . . . . . . !
!     SUBROUTINE fact3
! DESCRIPTION:
!     Computations for Exact MRBP.
! LANGUAGE:
!     Fortran 90/95
! AUTHOR:
!     Dr. Paul W. Mielke, Jr., Dept of Statistics, Colorado State University
!        http://www.stat.colostate.edu/~mielke/mielkesoftware.html
!     Modifications:
!        Jon D. Richards, Operations Research Analyst    jon_richards@usgs.gov
!        Fort Collins Science Center                  http://www.fort.usgs.gov
!        U.S. Geological Survey  -  Biological Resources Division
! INVOKED BY:
!     fact2     <File: "mrppmod.f90: fact2">     module
! INVOKES:
!     emrbpdval <File: "mrppmod.f90: emrbpdval"> module
!     fact4     <File: "mrppmod.f90: fact4">     module

!. . . . . . . . . . . . . . . SUBROUTINE FACT3 . . . . . . . . . . . . . . . !
!___Local Variables:
INTEGER, ALLOCATABLE :: ia_A(:,:)   ! dim(i_NumGrps,i_NumGrps)
INTEGER :: i, j, k, l, m, ii, jj, ll
INTEGER :: ios
INTEGER :: i_Flag, i_Kount3, i_Limit, i_NMarks, i_Temp
!___Intrinsic Procedures:
!__(none)__
!. . . . . . . . . . . . . . . SUBROUTINE FACT3 . . . . . . . . . . . . . . . !
!___Executable Statements:
   iEr = i_OK ! no problem yet
   ALLOCATE(ia_A(1:i_NumGrps,1:i_NumGrps), STAT=iEr)
   IF (iEr /= i_OK) THEN
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |_______
!  |                                          |
!  | 'Memory allocation error: ia_A in FACT3' |
!  |__________________________________________|
!
      i_CCode = iEr
      iEr = i_MRPPMOD_ER016
      RETURN   ! ... error exit ...
   END IF
   ia_A = 0 ! Initialize array.
   k = i_NumGrps
   l = i_NumGrps
   DO i=1,k-1
      ia_A(i,1) = i
      DO j=2,l
         ia_A(i,j) = i + 1
      END DO
      l = l - 1
   END DO
   i_Kount3 = 0
   ! 10    CONTINUE
   loop10: DO
      i = k - 1
      i_NMarks = 2
      IF (i_Kount3 == 0) GOTO 40
      !    20    CONTINUE
      loop20: DO
         DO j=1,i_NMarks-1
            IF (ia_A(i,j) == i) THEN
               IF (ia_A(i,j+1) == i+1) THEN
                  i_Limit = j - 2
                  ia_A(i,j) = ia_A(i,j+1)
                  ia_A(i,j+1) = ia_A(i,j) - 1
                  IF (i_Limit <= 0) GOTO 40
                  !                   GOTO 30
                  EXIT loop20
               END IF
            END IF
            call rchkusr()
         END DO
         IF (i_NMarks /= 1) THEN
            DO j=1,i_NMarks/2
               i_Temp = ia_A(i,j)
               ia_A(i,j) = ia_A(i,i_NMarks-j+1)
               ia_A(i,i_NMarks-j+1) = i_Temp
               call rchkusr()
            END DO
         END IF
         i = i - 1
         IF (i == 0) THEN
            DEALLOCATE(ia_A, STAT=ios)
            RETURN                        ! ... normal exit ...
         END IF
         i_NMarks = i_NMarks + 1
         !          GOTO 20
      END DO loop20
      !    30    CONTINUE
      i_Flag = 1
      DO WHILE (i_Flag==1)
         i_Flag = 0
         DO j=1,i_Limit
            IF (ia_A(i,j) == i+1) THEN
               IF (ia_A(i,j+1) == i) THEN
                  ia_A(i,j) = ia_A(i,j+1)
                  ia_A(i,j+1) = ia_A(i,j) + 1
                  i_Flag = 1
               END IF
            END IF
            call rchkusr()
         END DO
      END DO
40    CONTINUE
      ia_A(k,1:i_NumGrps) = ia_A(1,1:i_NumGrps)
      IF (k /= 2) THEN
         DO l=2,k-1
            m = 1
            DO j=1,i_NumGrps
               IF (ia_A(k,j) == l) THEN
                  ia_A(k,j) = ia_A(l,m)
                  m = m + 1
               END IF
               call rchkusr()
            END DO
         END DO
      END IF
      ll = 1
      DO ii=1,k
         DO jj=1,i_NumGrps
            IF (ia_A(k,jj) == ii) THEN
               ia_P3(ll) = jj
               ll = ll + 1
            END IF
            call rchkusr()
         END DO
      END DO
      IF (i_NumBlks == 3) THEN
         CALL emrbpdval(i_NumGrps, i_NumBlks, i_NumVars, i_KountB, d_Delta1,  &
               d_C, d_V, ia_P2, ia_P3, ia_P4, ia_P5, ia_P6, ia_P7, ia_P8,     &
               ia_P9, da_Data, iEr)  !* <File: "mrppmod.f90: emrbpdval">
         IF (iEr /= i_OK) THEN
            DEALLOCATE(ia_A, STAT=ios)
            RETURN   ! ... error exit ...
         END IF
         i_KountA = i_KountA + 1
      ELSE ! i_NumBlks is > 3, GOTO next FACT routine
         CALL fact4(i_NumGrps, i_NumBlks, i_NumVars, i_KountA, i_KountB,      &
               d_Delta1, d_C, d_V, ia_P2, ia_P3, ia_P4, ia_P5, ia_P6, ia_P7,  &
               ia_P8, ia_P9, da_Data, iEr) !* <File: "mrppmod.f90: fact4">
         IF (iEr /= i_OK) THEN
            DEALLOCATE(ia_A, STAT=ios)
            RETURN   ! ... error exit ...
         END IF
      END IF
      i_Kount3 = i_Kount3 + 1
      !       GOTO 10
   END DO loop10
END SUBROUTINE fact3

!=============================================================================!

SUBROUTINE fact4(i_NumGrps, i_NumBlks, i_NumVars, i_KountA, i_KountB,         &
      d_Delta1, d_C, d_V, ia_P2, ia_P3, ia_P4, ia_P5, ia_P6, ia_P7, ia_P8,    &
      ia_P9, da_Data, iEr)
!___Imported Parameters and Variables:
USE blcmnmod, ONLY: i_OK
        ! ^-- Some global Blossom variables and parameters.
USE jonsmodule, ONLY: i_CCode, i_MRPPMOD_ER017
        ! ^-- Message handling, etc.
!___Imported Procedures:
!__(none)__
IMPLICIT NONE
!___Dummy Arguments:
INTEGER,          INTENT(IN)    :: i_NumGrps ! # of Groups
INTEGER,          INTENT(IN)    :: i_NumBlks ! # of Blocks
INTEGER,          INTENT(IN)    :: i_NumVars ! # Resp. Vars
        ! i_KountA - count to total # permutations
INTEGER,          INTENT(INOUT) :: i_KountA
        ! i_KountB - count # times expected < observed MRBP statistic
INTEGER,          INTENT(INOUT) :: i_KountB
REAL (KIND(0D0)), INTENT(IN)    :: d_Delta1 ! Obs MRBP Stat
REAL (KIND(0D0)), INTENT(IN)    :: d_C
REAL (KIND(0D0)), INTENT(IN)    :: d_V      ! distance exponent
INTEGER,          INTENT(INOUT) :: ia_P2(:) ! dim(i_NumGrps)
INTEGER,          INTENT(INOUT) :: ia_P3(:) ! dim(i_NumGrps)
INTEGER,          INTENT(INOUT) :: ia_P4(:) ! dim(i_NumGrps)
INTEGER,          INTENT(INOUT) :: ia_P5(:) ! dim(i_NumGrps)
INTEGER,          INTENT(INOUT) :: ia_P6(:) ! dim(i_NumGrps)
INTEGER,          INTENT(INOUT) :: ia_P7(:) ! dim(i_NumGrps)
INTEGER,          INTENT(INOUT) :: ia_P8(:) ! dim(i_NumGrps)
INTEGER,          INTENT(INOUT) :: ia_P9(:) ! dim(i_NumGrps)
  ! da_Data - observation data - dim(i_NumGrps,i_NumBlks,i_NumVars)
REAL (KIND(0D0)), INTENT(INOUT) :: da_Data(:,:,:)
INTEGER,          INTENT(OUT)   :: iEr      ! error flag
!. . . . . . . . . . . . . . . SUBROUTINE FACT4 . . . . . . . . . . . . . . . !
!     SUBROUTINE fact4
! DESCRIPTION:
!     Computations for Exact MRBP.
! LANGUAGE:
!     Fortran 90/95
! AUTHOR:
!     Dr. Paul W. Mielke, Jr., Dept of Statistics, Colorado State University
!        http://www.stat.colostate.edu/~mielke/mielkesoftware.html
!     Modifications:
!        Jon D. Richards, Operations Research Analyst    jon_richards@usgs.gov
!        Fort Collins Science Center                  http://www.fort.usgs.gov
!        U.S. Geological Survey  -  Biological Resources Division
! INVOKED BY:
!     fact3     <File: "mrppmod.f90: fact3">     module
! INVOKES:
!     emrbpdval <File: "mrppmod.f90: emrbpdval"> module
!     fact5     <File: "mrppmod.f90: fact5">     module

!___Local Variables:
INTEGER, ALLOCATABLE :: ia_A(:,:)   ! dim(i_NumGrps,i_NumGrps)
INTEGER :: i, j, k, l, m, ii, jj, ll
INTEGER :: ios
INTEGER :: i_Flag, i_Kount4, i_Limit, i_NMarks, i_Temp
!___Intrinsic Procedures:
!__(none)__
!. . . . . . . . . . . . . . . SUBROUTINE FACT4 . . . . . . . . . . . . . . . !
!___Executable Statements:
   iEr = i_OK ! no problem yet
   ALLOCATE(ia_A(1:i_NumGrps,1:i_NumGrps), STAT=iEr)
   IF (iEr /= i_OK) THEN
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |_______
!  |                                          |
!  | 'Memory allocation error: ia_A in FACT4' |
!  |__________________________________________|
!
      i_CCode = iEr
      iEr = i_MRPPMOD_ER017
      RETURN   ! ... error exit ...
   END IF
   ia_A = 0 ! Initialize array.
   k = i_NumGrps
   l = i_NumGrps
   DO i=1,k-1
      ia_A(i,1) = i
      DO j=2,l
         ia_A(i,j) = i + 1
      END DO
      l = l - 1
   END DO
   i_Kount4 = 0
   ! 10    CONTINUE
   loop10: DO
      i = k - 1
      i_NMarks = 2
      IF (i_Kount4 == 0) GOTO 40
      !    20    CONTINUE
      loop20: DO
         DO j=1,i_NMarks-1
            IF (ia_A(i,j) == i) THEN
               IF (ia_A(i,j+1) == i+1) THEN
                  i_Limit = j - 2
                  ia_A(i,j) = ia_A(i,j+1)
                  ia_A(i,j+1) = ia_A(i,j) - 1
                  IF (i_Limit <= 0) GOTO 40
                  !                   GOTO 30
                  EXIT loop20
               END IF
            END IF
            call rchkusr()
         END DO
         IF (i_NMarks /= 1) THEN
            DO j=1,i_NMarks/2
               i_Temp = ia_A(i,j)
               ia_A(i,j) = ia_A(i,i_NMarks-j+1)
               ia_A(i,i_NMarks-j+1) = i_Temp
            END DO
         END IF
         i = i - 1
         IF (i == 0) THEN
            DEALLOCATE(ia_A, STAT=ios)
            RETURN                        ! ... normal exit ...
         END IF
         i_NMarks = i_NMarks + 1
         !          GOTO 20
      END DO loop20
      !    30    CONTINUE
      i_Flag = 1
      DO WHILE (i_Flag==1)
         i_Flag = 0
         DO j=1,i_Limit
            IF (ia_A(i,j) == i+1) THEN
               IF (ia_A(i,j+1) == i) THEN
                  ia_A(i,j) = ia_A(i,j+1)
                  ia_A(i,j+1) = ia_A(i,j) + 1
                  i_Flag = 1
               END IF
            END IF
         END DO
      END DO
40    CONTINUE
      ia_A(k,1:i_NumGrps) = ia_A(1,1:i_NumGrps)
      IF (k /= 2) THEN
         DO l=2,k-1
            m = 1
            DO j=1,i_NumGrps
               IF (ia_A(k,j) == l) THEN
                  ia_A(k,j) = ia_A(l,m)
                  m = m + 1
               END IF
               call rchkusr()
            END DO
         END DO
      END IF
      ll = 1
      DO ii=1,k
         DO jj=1,i_NumGrps
            IF (ia_A(k,jj) == ii) THEN
               ia_P4(ll) = jj
               ll = ll + 1
            END IF
            call rchkusr()
         END DO
      END DO
      IF (i_NumBlks == 4) THEN
         CALL emrbpdval(i_NumGrps, i_NumBlks, i_NumVars, i_KountB, d_Delta1,  &
               d_C, d_V, ia_P2, ia_P3, ia_P4, ia_P5, ia_P6, ia_P7, ia_P8,     &
               ia_P9, da_Data, iEr) !* <File: "mrppmod.f90: emrbpdval">
         IF (iEr /= i_OK) THEN
            DEALLOCATE(ia_A, STAT=ios)
            RETURN   ! ... error exit ...
         END IF
         i_KountA = i_KountA + 1
      ELSE ! i_NumBlks is > 4, GOTO next FACT routine
         CALL fact5(i_NumGrps, i_NumBlks, i_NumVars, i_KountA, i_KountB,      &
               d_Delta1, d_C, d_V, ia_P2, ia_P3, ia_P4, ia_P5, ia_P6, ia_P7,  &
               ia_P8, ia_P9, da_Data, iEr) !* <File: "mrppmod.f90: fact5">
         IF (iEr /= i_OK) THEN
            DEALLOCATE(ia_A, STAT=ios)
            RETURN   ! ... error exit ...
         END IF
      END IF
      i_Kount4 = i_Kount4 + 1
      !       GOTO 10
   END DO loop10
END SUBROUTINE fact4

!=============================================================================!

SUBROUTINE fact5(i_NumGrps, i_NumBlks, i_NumVars, i_KountA, i_KountB,         &
      d_Delta1, d_C, d_V, ia_P2, ia_P3, ia_P4, ia_P5, ia_P6, ia_P7, ia_P8,    &
      ia_P9, da_Data, iEr)
!___Imported Parameters and Variables:
USE blcmnmod, ONLY: i_OK
        ! ^-- Some global Blossom variables and parameters.
USE jonsmodule, ONLY: i_CCode, i_MRPPMOD_ER018
        ! ^-- Message handling, etc.
!___Imported Procedures:
!__(none)__
IMPLICIT NONE
!___Dummy Arguments:
INTEGER,          INTENT(IN)    :: i_NumGrps ! # of Groups
INTEGER,          INTENT(IN)    :: i_NumBlks ! # of Blocks
INTEGER,          INTENT(IN)    :: i_NumVars ! # Resp. Vars
        ! i_KountA - count to total # permutations
INTEGER,          INTENT(INOUT) :: i_KountA
        ! i_KountB - count # times expected < observed MRBP statistic
INTEGER,          INTENT(INOUT) :: i_KountB
REAL (KIND(0D0)), INTENT(IN)    :: d_Delta1 ! Obs MRBP Stat
REAL (KIND(0D0)), INTENT(IN)    :: d_C
REAL (KIND(0D0)), INTENT(IN)    :: d_V      ! distance exponent
INTEGER,          INTENT(INOUT) :: ia_P2(:) ! dim(i_NumGrps)
INTEGER,          INTENT(INOUT) :: ia_P3(:) ! dim(i_NumGrps)
INTEGER,          INTENT(INOUT) :: ia_P4(:) ! dim(i_NumGrps)
INTEGER,          INTENT(INOUT) :: ia_P5(:) ! dim(i_NumGrps)
INTEGER,          INTENT(INOUT) :: ia_P6(:) ! dim(i_NumGrps)
INTEGER,          INTENT(INOUT) :: ia_P7(:) ! dim(i_NumGrps)
INTEGER,          INTENT(INOUT) :: ia_P8(:) ! dim(i_NumGrps)
INTEGER,          INTENT(INOUT) :: ia_P9(:) ! dim(i_NumGrps)
  ! da_Data - observation data - dim(i_NumGrps,i_NumBlks,i_NumVars)
REAL (KIND(0D0)), INTENT(INOUT) :: da_Data(:,:,:)
INTEGER,          INTENT(OUT)   :: iEr      ! error flag
!. . . . . . . . . . . . . . . SUBROUTINE FACT5 . . . . . . . . . . . . . . . !
!     SUBROUTINE fact5
! DESCRIPTION:
!     Computations for Exact MRBP.
! LANGUAGE:
!     Fortran 90/95
! AUTHOR:
!     Dr. Paul W. Mielke, Jr., Dept of Statistics, Colorado State University
!        http://www.stat.colostate.edu/~mielke/mielkesoftware.html
!     Modifications:
!        Jon D. Richards, Operations Research Analyst    jon_richards@usgs.gov
!        Fort Collins Science Center                  http://www.fort.usgs.gov
!        U.S. Geological Survey  -  Biological Resources Division
! INVOKED BY:
!     fact4     <File: "mrppmod.f90: fact4">     module
! INVOKES:
!     emrbpdval <File: "mrppmod.f90: emrbpdval"> module
!     fact6     <File: "mrppmod.f90: fact6">     module


!. . . . . . . . . . . . . . . SUBROUTINE FACT5 . . . . . . . . . . . . . . . !
!___Local Variables:
INTEGER, ALLOCATABLE :: ia_A(:,:)   ! dim(i_NumGrps,i_NumGrps)
INTEGER :: i, j, k, l, m, ii, jj, ll
INTEGER :: ios
INTEGER :: i_Flag, i_Kount5, i_Limit, i_NMarks, i_Temp
!___Intrinsic Procedures:
!__(none)__
!. . . . . . . . . . . . . . . SUBROUTINE FACT5 . . . . . . . . . . . . . . . !
!___Executable Statements:
   iEr = i_OK ! no problem yet
   ALLOCATE(ia_A(1:i_NumGrps,1:i_NumGrps), STAT=iEr)
   IF (iEr /= i_OK) THEN
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |_______
!  |                                          |
!  | 'Memory allocation error: ia_A in FACT5' |
!  |__________________________________________|
!
      i_CCode = iEr
      iEr = i_MRPPMOD_ER018
      RETURN   ! ... error exit ...
   END IF
   ia_A = 0 ! Initialize array.
   k = i_NumGrps
   l = i_NumGrps
   DO i=1,k-1
      ia_A(i,1) = i
      DO j=2,l
         ia_A(i,j) = i + 1
      END DO
      l = l - 1
   END DO
   i_Kount5 = 0
   ! 10    CONTINUE
   loop10: DO
      i = k - 1
      i_NMarks = 2
      IF (i_Kount5 == 0) GOTO 40
      !    20    CONTINUE
      loop20: DO
         DO j=1,i_NMarks-1
            IF (ia_A(i,j) == i) THEN
               IF (ia_A(i,j+1) == i+1) THEN
                  i_Limit = j - 2
                  ia_A(i,j) = ia_A(i,j+1)
                  ia_A(i,j+1) = ia_A(i,j) - 1
                  IF (i_Limit <= 0) GOTO 40
                  !                   GOTO 30
                  EXIT loop20
               END IF
            END IF
         END DO
         IF (i_NMarks /= 1) THEN
            DO j=1,i_NMarks/2
               i_Temp = ia_A(i,j)
               ia_A(i,j) = ia_A(i,i_NMarks-j+1)
               ia_A(i,i_NMarks-j+1) = i_Temp
            END DO
         END IF
         i = i - 1
         IF (i == 0) THEN
            DEALLOCATE(ia_A, STAT=ios)
            RETURN                        ! ... normal exit ...
         END IF
         i_NMarks = i_NMarks + 1
         !          GOTO 20
      END DO loop20
      !    30    CONTINUE
      i_Flag = 1
      DO WHILE (i_Flag==1)
         i_Flag = 0
         DO j=1,i_Limit
            IF (ia_A(i,j) == i+1) THEN
               IF (ia_A(i,j+1) == i) THEN
                  ia_A(i,j) = ia_A(i,j+1)
                  ia_A(i,j+1) = ia_A(i,j) + 1
                  i_Flag = 1
               END IF
            END IF
         END DO
      END DO
40    CONTINUE
      ia_A(k,1:i_NumGrps) = ia_A(1,1:i_NumGrps)
      IF (k /= 2) THEN
         DO l=2,k-1
            m = 1
            DO j=1,i_NumGrps
               IF (ia_A(k,j) == l) THEN
                  ia_A(k,j) = ia_A(l,m)
                  m = m + 1
               END IF
            END DO
         END DO
      END IF
      ll = 1
      DO ii=1,k
         DO jj=1,i_NumGrps
            IF (ia_A(k,jj) == ii) THEN
               ia_P5(ll) = jj
               ll = ll + 1
            END IF
         END DO
      END DO
      IF (i_NumBlks == 5) THEN
         CALL emrbpdval(i_NumGrps, i_NumBlks, i_NumVars, i_KountB, d_Delta1,  &
               d_C, d_V, ia_P2, ia_P3, ia_P4, ia_P5, ia_P6, ia_P7, ia_P8,     &
               ia_P9, da_Data, iEr) !* <File: "mrppmod.f90: emrbpdval">
         IF (iEr /= i_OK) THEN
            DEALLOCATE(ia_A, STAT=ios)
            RETURN   ! ... error exit ...
         END IF
         i_KountA = i_KountA + 1
      ELSE ! i_NumBlks is > 5, GOTO next FACT routine
         CALL fact6(i_NumGrps, i_NumBlks, i_NumVars, i_KountA, i_KountB,      &
               d_Delta1, d_C, d_V, ia_P2, ia_P3, ia_P4, ia_P5, ia_P6, ia_P7,  &
               ia_P8, ia_P9, da_Data, iEr) !* <File: "mrppmod.f90: fact6">
         IF (iEr /= i_OK) THEN
            DEALLOCATE(ia_A, STAT=ios)
            RETURN   ! ... error exit ...
         END IF
      END IF
      i_Kount5 = i_Kount5 + 1
      !       GOTO 10
   END DO loop10
END SUBROUTINE fact5

!=============================================================================!

SUBROUTINE fact6(i_NumGrps, i_NumBlks, i_NumVars, i_KountA, i_KountB,         &
      d_Delta1, d_C, d_V, ia_P2, ia_P3, ia_P4, ia_P5, ia_P6, ia_P7, ia_P8,    &
      ia_P9, da_Data, iEr)
!___Imported Parameters and Variables:
USE blcmnmod, ONLY: i_OK
        ! ^-- Some global Blossom variables and parameters.
USE jonsmodule, ONLY: i_CCode, i_MRPPMOD_ER019
        ! ^-- Message handling, etc.
!___Imported Procedures:
!__(none)__
IMPLICIT NONE
!___Dummy Arguments:
INTEGER,          INTENT(IN)    :: i_NumGrps ! # of Groups
INTEGER,          INTENT(IN)    :: i_NumBlks ! # of Blocks
INTEGER,          INTENT(IN)    :: i_NumVars ! # Resp. Vars
        ! i_KountA - count to total # permutations
INTEGER,          INTENT(INOUT) :: i_KountA
        ! i_KountB - count # times expected < observed MRBP statistic
INTEGER,          INTENT(INOUT) :: i_KountB
REAL (KIND(0D0)), INTENT(IN)    :: d_Delta1 ! Obs MRBP Stat
REAL (KIND(0D0)), INTENT(IN)    :: d_C
REAL (KIND(0D0)), INTENT(IN)    :: d_V      ! distance exponent
INTEGER,          INTENT(INOUT) :: ia_P2(:) ! dim(i_NumGrps)
INTEGER,          INTENT(INOUT) :: ia_P3(:) ! dim(i_NumGrps)
INTEGER,          INTENT(INOUT) :: ia_P4(:) ! dim(i_NumGrps)
INTEGER,          INTENT(INOUT) :: ia_P5(:) ! dim(i_NumGrps)
INTEGER,          INTENT(INOUT) :: ia_P6(:) ! dim(i_NumGrps)
INTEGER,          INTENT(INOUT) :: ia_P7(:) ! dim(i_NumGrps)
INTEGER,          INTENT(INOUT) :: ia_P8(:) ! dim(i_NumGrps)
INTEGER,          INTENT(INOUT) :: ia_P9(:) ! dim(i_NumGrps)
  ! da_Data - observation data - dim(i_NumGrps,i_NumBlks,i_NumVars)
REAL (KIND(0D0)), INTENT(INOUT) :: da_Data(:,:,:)
INTEGER,          INTENT(OUT)   :: iEr      ! error flag
!. . . . . . . . . . . . . . . SUBROUTINE FACT6 . . . . . . . . . . . . . . . !
!     SUBROUTINE fact6
! DESCRIPTION:
!     Computations for Exact MRBP.
! LANGUAGE:
!     Fortran 90/95
! AUTHOR:
!     Dr. Paul W. Mielke, Jr., Dept of Statistics, Colorado State University
!        http://www.stat.colostate.edu/~mielke/mielkesoftware.html
!     Modifications:
!        Jon D. Richards, Operations Research Analyst    jon_richards@usgs.gov
!        Fort Collins Science Center                  http://www.fort.usgs.gov
!        U.S. Geological Survey  -  Biological Resources Division

!. . . . . . . . . . . . . . . SUBROUTINE FACT6 . . . . . . . . . . . . . . . !
!___Local Variables:
INTEGER, ALLOCATABLE :: ia_A(:,:)   ! dim(i_NumGrps,i_NumGrps)
INTEGER :: i, j, k, l, m, ii, jj, ll
INTEGER :: ios
INTEGER :: i_Flag, i_Kount6, i_Limit, i_NMarks, i_Temp
!___Intrinsic Procedures:
!__(none)__
!. . . . . . . . . . . . . . . SUBROUTINE FACT6 . . . . . . . . . . . . . . . !
!___Executable Statements:
   iEr = i_OK ! no problem yet
   ALLOCATE(ia_A(1:i_NumGrps,1:i_NumGrps), STAT=iEr)
   IF (iEr /= i_OK) THEN
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |_______
!  |                                          |
!  | 'Memory allocation error: ia_A in FACT6' |
!  |__________________________________________|
!
      i_CCode = iEr
      iEr = i_MRPPMOD_ER019
      RETURN   ! ... error exit ...
   END IF
   ia_A = 0 ! Initialize array.
   k = i_NumGrps
   l = i_NumGrps
   DO i=1,k-1
      ia_A(i,1) = i
      DO j=2,l
         ia_A(i,j) = i + 1
      END DO
      l = l - 1
   END DO
   i_Kount6 = 0
   ! 10    CONTINUE
   loop10: DO
      i = k - 1
      i_NMarks = 2
      IF (i_Kount6 == 0) GOTO 40
      !    20    CONTINUE
      loop20: DO
         DO j=1,i_NMarks-1
            IF (ia_A(i,j) == i) THEN
               IF (ia_A(i,j+1) == i+1) THEN
                  i_Limit = j - 2
                  ia_A(i,j) = ia_A(i,j+1)
                  ia_A(i,j+1) = ia_A(i,j) - 1
                  IF (i_Limit <= 0) GOTO 40
                  !                   GOTO 30
                  EXIT loop20
               END IF
            END IF
         END DO
         IF (i_NMarks /= 1) THEN
            DO j=1,i_NMarks/2
               i_Temp = ia_A(i,j)
               ia_A(i,j) = ia_A(i,i_NMarks-j+1)
               ia_A(i,i_NMarks-j+1) = i_Temp
            END DO
         END IF
         i = i - 1
         IF (i == 0) THEN
            DEALLOCATE(ia_A, STAT=ios)
            RETURN                        ! ... normal exit ...
         END IF
         i_NMarks = i_NMarks + 1
         !       GOTO 20
      END DO loop20
      !    30    CONTINUE
      i_Flag = 1
      DO WHILE (i_Flag==1)
         i_Flag = 0
         DO j=1,i_Limit
            IF (ia_A(i,j) == i+1) THEN
               IF (ia_A(i,j+1) == i) THEN
                  ia_A(i,j) = ia_A(i,j+1)
                  ia_A(i,j+1) = ia_A(i,j) + 1
                  i_Flag = 1
               END IF
            END IF
         END DO
      END DO
40    CONTINUE
      ia_A(k,1:i_NumGrps) = ia_A(1,1:i_NumGrps)
      IF (k /= 2) THEN
         DO l=2,k-1
            m = 1
            DO j=1,i_NumGrps
               IF (ia_A(k,j) == l) THEN
                  ia_A(k,j) = ia_A(l,m)
                  m = m + 1
               END IF
            END DO
         END DO
      END IF
      ll = 1
      DO ii=1,k
         DO jj=1,i_NumGrps
            IF (ia_A(k,jj) == ii) THEN
               ia_P6(ll) = jj
               ll = ll + 1
            END IF
         END DO
      END DO
      IF (i_NumBlks == 6) THEN
         CALL emrbpdval(i_NumGrps, i_NumBlks, i_NumVars, i_KountB, d_Delta1,  &
               d_C, d_V, ia_P2, ia_P3, ia_P4, ia_P5, ia_P6, ia_P7, ia_P8,     &
               ia_P9, da_Data, iEr) !* <File: "mrppmod.f90: emrbpdval">
         IF (iEr /= i_OK) THEN
            DEALLOCATE(ia_A, STAT=ios)
            RETURN   ! ... error exit ...
         END IF
         i_KountA = i_KountA + 1
      ELSE ! i_NumBlks is > 6, GOTO next FACT routine
         CALL fact7(i_NumGrps, i_NumBlks, i_NumVars, i_KountA, i_KountB,      &
               d_Delta1, d_C, d_V, ia_P2, ia_P3, ia_P4, ia_P5, ia_P6, ia_P7,  &
               ia_P8, ia_P9, da_Data, iEr) !* <File: "mrppmod.f90: fact7">
         IF (iEr /= i_OK) THEN
            DEALLOCATE(ia_A, STAT=ios)
            RETURN   ! ... error exit ...
         END IF
      END IF
      i_Kount6 = i_Kount6 + 1
      !       GOTO 10
   END DO loop10
END SUBROUTINE fact6

!=============================================================================!

SUBROUTINE fact7(i_NumGrps, i_NumBlks, i_NumVars, i_KountA, i_KountB,         &
      d_Delta1, d_C, d_V, ia_P2, ia_P3, ia_P4, ia_P5, ia_P6, ia_P7, ia_P8,    &
      ia_P9, da_Data, iEr)
!___Imported Parameters and Variables:
USE blcmnmod, ONLY: i_OK
        ! ^-- Some global Blossom variables and parameters.
USE jonsmodule, ONLY: i_CCode, i_MRPPMOD_ER020
        ! ^-- Message handling, etc.
!___Imported Procedures:
!__(none)__
IMPLICIT NONE
!___Dummy Arguments:
INTEGER,          INTENT(IN)    :: i_NumGrps ! # of Groups
INTEGER,          INTENT(IN)    :: i_NumBlks ! # of Blocks
INTEGER,          INTENT(IN)    :: i_NumVars ! # Resp. Vars
        ! i_KountA - count to total # permutations
INTEGER,          INTENT(INOUT) :: i_KountA
        ! i_KountB - count # times expected < observed MRBP statistic
INTEGER,          INTENT(INOUT) :: i_KountB
REAL (KIND(0D0)), INTENT(IN)    :: d_Delta1 ! Obs MRBP Stat
REAL (KIND(0D0)), INTENT(IN)    :: d_C
REAL (KIND(0D0)), INTENT(IN)    :: d_V      ! distance exponent
INTEGER,          INTENT(INOUT) :: ia_P2(:) ! dim(i_NumGrps)
INTEGER,          INTENT(INOUT) :: ia_P3(:) ! dim(i_NumGrps)
INTEGER,          INTENT(INOUT) :: ia_P4(:) ! dim(i_NumGrps)
INTEGER,          INTENT(INOUT) :: ia_P5(:) ! dim(i_NumGrps)
INTEGER,          INTENT(INOUT) :: ia_P6(:) ! dim(i_NumGrps)
INTEGER,          INTENT(INOUT) :: ia_P7(:) ! dim(i_NumGrps)
INTEGER,          INTENT(INOUT) :: ia_P8(:) ! dim(i_NumGrps)
INTEGER,          INTENT(INOUT) :: ia_P9(:) ! dim(i_NumGrps)
  ! da_Data - observation data - dim(i_NumGrps,i_NumBlks,i_NumVars)
REAL (KIND(0D0)), INTENT(INOUT) :: da_Data(:,:,:)
INTEGER,          INTENT(OUT)   :: iEr      ! error flag
!. . . . . . . . . . . . . . . SUBROUTINE FACT7 . . . . . . . . . . . . . . . !
!     SUBROUTINE fact7
! DESCRIPTION:
!     Computations for Exact MRBP.
! LANGUAGE:
!     Fortran 90/95
! AUTHOR:
!     Dr. Paul W. Mielke, Jr., Dept of Statistics, Colorado State University
!        http://www.stat.colostate.edu/~mielke/mielkesoftware.html
!     Modifications:
!        Jon D. Richards, Operations Research Analyst    jon_richards@usgs.gov
!        Fort Collins Science Center                  http://www.fort.usgs.gov
!        U.S. Geological Survey  -  Biological Resources Division
! INVOKED BY:
!     fact6     <File: "mrppmod.f90: fact6">     module
! INVOKES:
!     emrbpdval <File: "mrppmod.f90: emrbpdval"> module
!     fact8     <File: "mrppmod.f90: fact8">     module
! FILES:
!     -
! DEVICES:
!     -

!. . . . . . . . . . . . . . . SUBROUTINE FACT7 . . . . . . . . . . . . . . . !
!___Local Variables:
INTEGER, ALLOCATABLE :: ia_A(:,:)   ! dim(i_NumGrps,i_NumGrps)
INTEGER :: i, j, k, l, m, ii, jj, ll
INTEGER :: ios
INTEGER :: i_Flag, i_Kount7, i_Limit, i_NMarks, i_Temp
!___Intrinsic Procedures:
!__(none)__
!. . . . . . . . . . . . . . . SUBROUTINE FACT7 . . . . . . . . . . . . . . . !
!___Executable Statements:
   iEr = i_OK ! no problem yet
   ALLOCATE(ia_A(1:i_NumGrps,1:i_NumGrps), STAT=iEr)
   IF (iEr /= i_OK) THEN
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |_______
!  |                                          |
!  | 'Memory allocation error: ia_A in FACT7' |
!  |__________________________________________|
!
      i_CCode = iEr
      iEr = i_MRPPMOD_ER020
      RETURN   ! ... error exit ...
   END IF
   ia_A = 0 ! Initialize array.
   k = i_NumGrps
   l = i_NumGrps
   DO i=1,k-1
      ia_A(i,1) = i
      DO j=2,l
         ia_A(i,j) = i + 1
      END DO
      l = l - 1
   END DO
   i_Kount7 = 0
   ! 10    CONTINUE
   loop10: DO
      i = k - 1
      i_NMarks = 2
      IF (i_Kount7 == 0) GOTO 40
      !    20    CONTINUE
      loop20: DO
         DO j=1,i_NMarks-1
            IF (ia_A(i,j) == i) THEN
               IF (ia_A(i,j+1) == i+1) THEN
                  i_Limit = j - 2
                  ia_A(i,j) = ia_A(i,j+1)
                  ia_A(i,j+1) = ia_A(i,j) - 1
                  IF (i_Limit <= 0) GOTO 40
                  !                   GOTO 30
                  EXIT loop20
               END IF
            END IF
         END DO
         IF (i_NMarks /= 1) THEN
            DO j=1,i_NMarks/2
               i_Temp = ia_A(i,j)
               ia_A(i,j) = ia_A(i,i_NMarks-j+1)
               ia_A(i,i_NMarks-j+1) = i_Temp
            END DO
         END IF
         i = i - 1
         IF (i == 0) THEN
            DEALLOCATE(ia_A, STAT=ios)
            RETURN                        ! ... normal exit ...
         END IF
         i_NMarks = i_NMarks + 1
         !       GOTO 20
      END DO loop20
      !    30    CONTINUE
      i_Flag = 1
      DO WHILE (i_Flag==1)
         i_Flag = 0
         DO j=1,i_Limit
            IF (ia_A(i,j) == i+1) THEN
               IF (ia_A(i,j+1) == i) THEN
                  ia_A(i,j) = ia_A(i,j+1)
                  ia_A(i,j+1) = ia_A(i,j) + 1
                  i_Flag = 1
               END IF
            END IF
         END DO
      END DO
40    CONTINUE
      ia_A(k,1:i_NumGrps) = ia_A(1,1:i_NumGrps)
      IF (k /= 2) THEN
         DO l=2,k-1
            m = 1
            DO j=1,i_NumGrps
               IF (ia_A(k,j) == l) THEN
                  ia_A(k,j) = ia_A(l,m)
                  m = m + 1
               END IF
            END DO
         END DO
      END IF
      ll = 1
      DO ii=1,k
         DO jj=1,i_NumGrps
            IF (ia_A(k,jj) == ii) THEN
               ia_P7(ll) = jj
               ll = ll + 1
            END IF
         END DO
      END DO
      IF (i_NumBlks == 7) THEN
         CALL emrbpdval(i_NumGrps, i_NumBlks, i_NumVars, i_KountB, d_Delta1,  &
               d_C, d_V, ia_P2, ia_P3, ia_P4, ia_P5, ia_P6, ia_P7, ia_P8,     &
               ia_P9, da_Data, iEr) !* <File: "mrppmod.f90: emrbpdval">
         IF (iEr /= i_OK) THEN
            DEALLOCATE(ia_A, STAT=ios)
            RETURN   ! ... error exit ...
         END IF
         i_KountA = i_KountA + 1
      ELSE ! i_NumBlks is > 7, GOTO next FACT routine
         CALL fact8(i_NumGrps, i_NumBlks, i_NumVars, i_KountA, i_KountB,      &
               d_Delta1, d_C, d_V, ia_P2, ia_P3, ia_P4, ia_P5, ia_P6, ia_P7,  &
               ia_P8, ia_P9, da_Data, iEr) !* <File: "mrppmod.f90: fact8">
         IF (iEr /= i_OK) THEN
            DEALLOCATE(ia_A, STAT=ios)
            RETURN   ! ... error exit ...
         END IF
      END IF
      i_Kount7 = i_Kount7 + 1
      !    GOTO 10
   END DO loop10
END SUBROUTINE fact7

!=============================================================================!

SUBROUTINE fact8(i_NumGrps, i_NumBlks, i_NumVars, i_KountA, i_KountB,         &
      d_Delta1, d_C, d_V, ia_P2, ia_P3, ia_P4, ia_P5, ia_P6, ia_P7, ia_P8,    &
      ia_P9, da_Data, iEr)
!___Imported Parameters and Variables:
USE blcmnmod, ONLY: i_OK
        ! ^-- Some global Blossom variables and parameters.
USE jonsmodule, ONLY: i_CCode, i_MRPPMOD_ER021
        ! ^-- Message handling, etc.
!___Imported Procedures:
!__(none)__
IMPLICIT NONE
!___Dummy Arguments:
INTEGER,          INTENT(IN)    :: i_NumGrps ! # of Groups
INTEGER,          INTENT(IN)    :: i_NumBlks ! # of Blocks
INTEGER,          INTENT(IN)    :: i_NumVars ! # Resp. Vars
        ! i_KountA - count to total # permutations
INTEGER,          INTENT(INOUT) :: i_KountA
        ! i_KountB - count # times expected < observed MRBP statistic
INTEGER,          INTENT(INOUT) :: i_KountB
REAL (KIND(0D0)), INTENT(IN)    :: d_Delta1 ! Obs MRBP Stat
REAL (KIND(0D0)), INTENT(IN)    :: d_C
REAL (KIND(0D0)), INTENT(IN)    :: d_V      ! distance exponent
INTEGER,          INTENT(INOUT) :: ia_P2(:) ! dim(i_NumGrps)
INTEGER,          INTENT(INOUT) :: ia_P3(:) ! dim(i_NumGrps)
INTEGER,          INTENT(INOUT) :: ia_P4(:) ! dim(i_NumGrps)
INTEGER,          INTENT(INOUT) :: ia_P5(:) ! dim(i_NumGrps)
INTEGER,          INTENT(INOUT) :: ia_P6(:) ! dim(i_NumGrps)
INTEGER,          INTENT(INOUT) :: ia_P7(:) ! dim(i_NumGrps)
INTEGER,          INTENT(INOUT) :: ia_P8(:) ! dim(i_NumGrps)
INTEGER,          INTENT(INOUT) :: ia_P9(:) ! dim(i_NumGrps)
  ! da_Data - observation data - dim(i_NumGrps,i_NumBlks,i_NumVars)
REAL (KIND(0D0)), INTENT(INOUT) :: da_Data(:,:,:)
INTEGER,          INTENT(OUT)   :: iEr      ! error flag
!. . . . . . . . . . . . . . . SUBROUTINE FACT8 . . . . . . . . . . . . . . . !
!     SUBROUTINE fact8
! DESCRIPTION:
!     Computations for Exact MRBP.
! LANGUAGE:
!     Fortran 90/95
! AUTHOR:
!     Dr. Paul W. Mielke, Jr., Dept of Statistics, Colorado State University
!        http://www.stat.colostate.edu/~mielke/mielkesoftware.html
!     Modifications:
!        Jon D. Richards, Operations Research Analyst    jon_richards@usgs.gov
!        Fort Collins Science Center                  http://www.fort.usgs.gov
!        U.S. Geological Survey  -  Biological Resources Division
! INVOKED BY:
!     fact7     <File: "mrppmod.f90: fact7">     module
! INVOKES:
!     emrbpdval <File: "mrppmod.f90: emrbpdval"> module
!     fact9     <File: "mrppmod.f90: fact9">     module

!. . . . . . . . . . . . . . . SUBROUTINE FACT8 . . . . . . . . . . . . . . . !
!___Local Variables:
INTEGER, ALLOCATABLE :: ia_A(:,:)   ! dim(i_NumGrps,i_NumGrps)
INTEGER :: i, j, k, l, m, ii, jj, ll
INTEGER :: ios
INTEGER :: i_Flag, i_Kount8, i_Limit, i_NMarks, i_Temp
!___Intrinsic Procedures:
!__(none)__
!. . . . . . . . . . . . . . . SUBROUTINE FACT8 . . . . . . . . . . . . . . . !
!___Executable Statements:
   iEr = i_OK ! no problem yet
   ALLOCATE(ia_A(1:i_NumGrps,1:i_NumGrps), STAT=iEr)
   IF (iEr /= i_OK) THEN
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |_______
!  |                                          |
!  | 'Memory allocation error: ia_A in FACT8' |
!  |__________________________________________|
!
      i_CCode = iEr
      iEr = i_MRPPMOD_ER021
      RETURN   ! ... error exit ...
   END IF
   ia_A = 0 ! Initialize array.
   k = i_NumGrps
   l = i_NumGrps
   DO i=1,k-1
      ia_A(i,1) = i
      DO j=2,l
         ia_A(i,j) = i + 1
      END DO
      l = l - 1
   END DO
   i_Kount8 = 0
   ! 10    CONTINUE
   loop10: DO
      i = k - 1
      i_NMarks = 2
      IF (i_Kount8 == 0) GOTO 40
      !    20    CONTINUE
      loop20: DO
         DO j=1,i_NMarks-1
            IF (ia_A(i,j) == i) THEN
               IF (ia_A(i,j+1) == i+1) THEN
                  i_Limit = j - 2
                  ia_A(i,j)   = ia_A(i,j+1)
                  ia_A(i,j+1) = ia_A(i,j) - 1
                  IF (i_Limit <= 0) GOTO 40
                  !                   GOTO 30
                  EXIT loop20
               END IF
            END IF
         END DO
         IF (i_NMarks /= 1) THEN
            DO j=1,i_NMarks/2
               i_Temp = ia_A(i,j)
               ia_A(i,j) = ia_A(i,i_NMarks-j+1)
               ia_A(i,i_NMarks-j+1) = i_Temp
            END DO
         END IF
         i = i - 1
         IF (i == 0) THEN
            DEALLOCATE(ia_A, STAT=ios)
            RETURN                        ! ... normal exit ...
         END IF
         i_NMarks = i_NMarks + 1
         !          GOTO 20
      END DO loop20
      !    30    CONTINUE
      i_Flag = 1
      DO WHILE (i_Flag==1)
         i_Flag = 0
         DO j=1,i_Limit
            IF (ia_A(i,j) == i+1) THEN
               IF (ia_A(i,j+1) == i) THEN
                  ia_A(i,j)   = ia_A(i,j+1)
                  ia_A(i,j+1) = ia_A(i,j) + 1
                  i_Flag = 1
               END IF
            END IF
         END DO
      END DO
40    CONTINUE
      ia_A(k,1:i_NumGrps) = ia_A(1,1:i_NumGrps)
      IF (k /= 2) THEN
         DO l=2,k-1
            m = 1
            DO j=1,i_NumGrps
               IF (ia_A(k,j) == l) THEN
                  ia_A(k,j) = ia_A(l,m)
                  m = m + 1
               END IF
            END DO
         END DO
      END IF
      ll = 1
      DO ii=1,k
         DO jj=1,i_NumGrps
            IF (ia_A(k,jj) == ii) THEN
               ia_P8(ll) = jj
               ll = ll + 1
            END IF
         END DO
      END DO
      IF (i_NumBlks == 8) THEN
         CALL emrbpdval(i_NumGrps, i_NumBlks, i_NumVars, i_KountB, d_Delta1,  &
               d_C, d_V, ia_P2, ia_P3, ia_P4, ia_P5, ia_P6, ia_P7, ia_P8,     &
               ia_P9, da_Data, iEr) !* <File: "mrppmod.f90: emrbpdval">
         IF (iEr /= i_OK) THEN
            DEALLOCATE(ia_A, STAT=ios)
            RETURN   ! ... error exit ...
         END IF
         i_KountA = i_KountA + 1
      ELSE  ! i_NumBlks is 9
         CALL fact9(i_NumGrps, i_NumBlks, i_NumVars, i_KountA, i_KountB,      &
               d_Delta1, d_C, d_V, ia_P2, ia_P3, ia_P4, ia_P5, ia_P6, ia_P7,  &
               ia_P8, ia_P9, da_Data, iEr) !* <File: "mrppmod.f90: fact9">
         IF (iEr /= i_OK) THEN
            DEALLOCATE(ia_A, STAT=ios)
            RETURN   ! ... error exit ...
         END IF
      END IF
      i_Kount8 = i_Kount8 + 1
      !       GOTO 10
   END DO loop10
END SUBROUTINE fact8

!=============================================================================!

SUBROUTINE fact9(i_NumGrps, i_NumBlks, i_NumVars, i_KountA, i_KountB,         &
      d_Delta1, d_C, d_V, ia_P2, ia_P3, ia_P4, ia_P5, ia_P6, ia_P7, ia_P8,    &
      ia_P9, da_Data, iEr)
!___Imported Parameters and Variables:
USE blcmnmod, ONLY: i_OK
        ! ^-- Some global Blossom variables and parameters.
USE jonsmodule, ONLY: i_CCode, i_MRPPMOD_ER022
        ! ^-- Message handling, etc.
!___Imported Procedures:
!__(none)__
IMPLICIT NONE
!___Dummy Arguments:
INTEGER,          INTENT(IN)    :: i_NumGrps ! # of Groups
INTEGER,          INTENT(IN)    :: i_NumBlks ! # of Blocks
INTEGER,          INTENT(IN)    :: i_NumVars ! # Resp. Vars
        ! i_KountA - count to total # permutations
INTEGER,          INTENT(INOUT) :: i_KountA
        ! i_KountB - count # times expected < observed MRBP statistic
INTEGER,          INTENT(INOUT) :: i_KountB
REAL (KIND(0D0)), INTENT(IN)    :: d_Delta1 ! Obs MRBP Stat
REAL (KIND(0D0)), INTENT(IN)    :: d_C
REAL (KIND(0D0)), INTENT(IN)    :: d_V      ! distance exponent
INTEGER,          INTENT(INOUT) :: ia_P2(:) ! dim(i_NumGrps)
INTEGER,          INTENT(INOUT) :: ia_P3(:) ! dim(i_NumGrps)
INTEGER,          INTENT(INOUT) :: ia_P4(:) ! dim(i_NumGrps)
INTEGER,          INTENT(INOUT) :: ia_P5(:) ! dim(i_NumGrps)
INTEGER,          INTENT(INOUT) :: ia_P6(:) ! dim(i_NumGrps)
INTEGER,          INTENT(INOUT) :: ia_P7(:) ! dim(i_NumGrps)
INTEGER,          INTENT(INOUT) :: ia_P8(:) ! dim(i_NumGrps)
INTEGER,          INTENT(INOUT) :: ia_P9(:) ! dim(i_NumGrps)
  ! da_Data - observation data - dim(i_NumGrps,i_NumBlks,i_NumVars)
REAL (KIND(0D0)), INTENT(INOUT) :: da_Data(:,:,:)
INTEGER,          INTENT(OUT)   :: iEr      ! error flag
!. . . . . . . . . . . . . . . SUBROUTINE FACT9 . . . . . . . . . . . . . . . !
!     SUBROUTINE fact9
! DESCRIPTION:
!     Computations for Exact MRBP.
! LANGUAGE:
!     Fortran 90/95
! AUTHOR:
!     Dr. Paul W. Mielke, Jr., Dept of Statistics, Colorado State University
!        http://www.stat.colostate.edu/~mielke/mielkesoftware.html
!     Modifications:
!        Jon D. Richards, Operations Research Analyst    jon_richards@usgs.gov
!        Fort Collins Science Center                  http://www.fort.usgs.gov
!        U.S. Geological Survey  -  Biological Resources Division
! INVOKED BY:
!     fact8     <File: "mrppmod.f90: fact8">     module
! INVOKES:
!     emrbpdval <File: "mrppmod.f90: emrbpdval"> module

!. . . . . . . . . . . . . . . SUBROUTINE FACT9 . . . . . . . . . . . . . . . !
!___Local Variables:
INTEGER, ALLOCATABLE :: ia_A(:,:)   ! dim(i_NumGrps,i_NumGrps)
INTEGER :: i, j, k, l, m, ii, jj, ll
INTEGER :: ios
INTEGER :: i_Flag, i_Kount9, i_Limit, i_NMarks, i_Temp
!___Intrinsic Procedures:
!__(none)__
!. . . . . . . . . . . . . . . SUBROUTINE FACT9 . . . . . . . . . . . . . . . !
!___Executable Statements:
   iEr = i_OK ! no problem yet
   ALLOCATE(ia_A(1:i_NumGrps,1:i_NumGrps), STAT=iEr)
   IF (iEr /= i_OK) THEN
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |_______
!  |                                          |
!  | 'Memory allocation error: ia_A in FACT9' |
!  |__________________________________________|
!
      i_CCode = iEr
      iEr = i_MRPPMOD_ER022
      RETURN   ! ... error exit ...
   END IF
   ia_A = 0 ! Initialize array.
   k = i_NumGrps
   l = i_NumGrps
   DO i=1,k-1
      ia_A(i,1) = i
      DO j=2,l
         ia_A(i,j) = i + 1
      END DO
      l = l - 1
   END DO
   i_Kount9 = 0
   ! 10    CONTINUE
   loop10: DO
      i = k - 1
      i_NMarks = 2
      IF (i_Kount9 == 0) GOTO 40
      !    20    CONTINUE
      loop20: DO
         DO j=1,i_NMarks-1
            IF (ia_A(i,j) == i) THEN
               IF (ia_A(i,j+1) == i+1) THEN
                  i_Limit = j - 2
                  ia_A(i,j)   = ia_A(i,j+1)
                  ia_A(i,j+1) = ia_A(i,j) - 1
                  IF (i_Limit <= 0) GOTO 40
                  !                   GOTO 30
                  EXIT loop20
               END IF
            END IF
         END DO
         IF (i_NMarks /= 1) THEN
            DO j=1,i_NMarks/2
               i_Temp    = ia_A(i,j)
               ia_A(i,j) = ia_A(i,i_NMarks-j+1)
               ia_A(i,i_NMarks-j+1) = i_Temp
            END DO
         END IF
         i = i - 1
         IF (i == 0) THEN
            DEALLOCATE(ia_A, STAT=ios)
            RETURN                        ! ... normal exit ...
         END IF
         i_NMarks = i_NMarks + 1
          !          GOTO 20
      END DO loop20
      !    30    CONTINUE
      i_Flag = 1
      DO WHILE (i_Flag==1)
         i_Flag = 0
         DO j=1,i_Limit
            IF (ia_A(i,j) == i+1) THEN
               IF (ia_A(i,j+1) == i) THEN
                  ia_A(i,j)   = ia_A(i,j+1)
                  ia_A(i,j+1) = ia_A(i,j) + 1
                  i_Flag = 1
               END IF
            END IF
         END DO
      END DO
40    CONTINUE
      ia_A(k,1:i_NumGrps) = ia_A(1,1:i_NumGrps)
      IF (k /= 2) THEN
         DO l=2,k-1
            m = 1
            DO j=1,i_NumGrps
               IF (ia_A(k,j) == l) THEN
                  ia_A(k,j) = ia_A(l,m)
                  m = m + 1
               END IF
            END DO
         END DO
      END IF
      ll = 1
      DO ii=1,k
         DO jj=1,i_NumGrps
            IF (ia_A(k,jj) == ii) THEN
               ia_P9(ll) = jj
               ll = ll + 1
            END IF
         END DO
      END DO
      CALL emrbpdval(i_NumGrps, i_NumBlks, i_NumVars, i_KountB, d_Delta1,     &
            d_C, d_V, ia_P2, ia_P3, ia_P4, ia_P5, ia_P6, ia_P7, ia_P8, ia_P9, &
            da_Data, iEr) !* <File: "mrppmod.f90: emrbpdval">
      IF (iEr /= i_OK) THEN
         DEALLOCATE(ia_A, STAT=ios)
         RETURN   ! ... error exit ...
      END IF
      i_Kount9 = i_Kount9 + 1
      i_KountA = i_KountA + 1
      !       GOTO 10
   END DO loop10
END SUBROUTINE fact9

!=============================================================================!

SUBROUTINE emrbpdval(i_NumGrps, i_NumBlks, i_NumVars, i_KountB, d_Delta1,     &
      d_C, d_V, ia_P2, ia_P3, ia_P4, ia_P5, ia_P6, ia_P7, ia_P8, ia_P9,       &
      da_Data, iEr)
!___Imported Parameters and Variables:
USE blcmnmod, ONLY: d_TWO, d_ZERO, i_OK
        ! ^-- Some global Blossom variables and parameters.
USE jonsmodule, ONLY: i_CCode, i_MRPPMOD_ER023
        ! ^-- Message handling, etc.
!___Imported Procedures:
!__(none)__
!. . . . . . . . . . . . . . . SUBROUTINE emrbpdval . . . . . . . . . . . . . !
IMPLICIT NONE
!___Dummy Arguments:
INTEGER,          INTENT(IN)    :: i_NumGrps ! # of Groups
INTEGER,          INTENT(IN)    :: i_NumBlks ! # of Blocks
INTEGER,          INTENT(IN)    :: i_NumVars ! # ResV
        ! i_KountB count # times expected < observed MRBP statistic
INTEGER,          INTENT(INOUT) :: i_KountB
REAL (KIND(0D0)), INTENT(IN)    :: d_Delta1  ! Observ MRBP Stat
REAL (KIND(0D0)), INTENT(IN)    :: d_C
REAL (KIND(0D0)), INTENT(IN)    :: d_V       ! distance exponent
INTEGER,          INTENT(INOUT) :: ia_P2(:)  ! dim(i_NumGrps)
INTEGER,          INTENT(INOUT) :: ia_P3(:)  ! dim(i_NumGrps)
INTEGER,          INTENT(INOUT) :: ia_P4(:)  ! dim(i_NumGrps)
INTEGER,          INTENT(INOUT) :: ia_P5(:)  ! dim(i_NumGrps)
INTEGER,          INTENT(INOUT) :: ia_P6(:)  ! dim(i_NumGrps)
INTEGER,          INTENT(INOUT) :: ia_P7(:)  ! dim(i_NumGrps)
INTEGER,          INTENT(INOUT) :: ia_P8(:)  ! dim(i_NumGrps)
INTEGER,          INTENT(INOUT) :: ia_P9(:)  ! dim(i_NumGrps)
        ! da_Data - observation data - dim(i_NumGrps,i_NumBlks,i_NumVars)
REAL (KIND(0D0)), INTENT(INOUT) :: da_Data(:,:,:)
INTEGER,          INTENT(OUT)   :: iEr       ! error flag
!. . . . . . . . . . . . . . . SUBROUTINE emrbpdval . . . . . . . . . . . . . !
!     SUBROUTINE emrbpdval
! DESCRIPTION:
!     Computations for Exact MRBP.
! LANGUAGE:
!     Fortran 90/95
! AUTHOR:
!     Dr. Paul W. Mielke, Jr., Dept of Statistics, Colorado State University
!        http://www.stat.colostate.edu/~mielke/mielkesoftware.html
!     Modifications:
!        Jon D. Richards, Operations Research Analyst    jon_richards@usgs.gov
!        Fort Collins Science Center                  http://www.fort.usgs.gov
!        U.S. Geological Survey  -  Biological Resources Division
! INVOKED BY:
!     fact2 <File: "mrppmod.f90: fact2"> module
!     fact3 <File: "mrppmod.f90: fact3"> module
!     fact4 <File: "mrppmod.f90: fact4"> module
!     fact5 <File: "mrppmod.f90: fact5"> module
!     fact6 <File: "mrppmod.f90: fact6"> module
!     fact7 <File: "mrppmod.f90: fact7"> module
!     fact8 <File: "mrppmod.f90: fact8"> module
!     fact9 <File: "mrppmod.f90: fact9"> module
! INVOKES:
!     -

!. . . . . . . . . . . . . . . SUBROUTINE emrbpdval . . . . . . . . . . . . . !
!___Local Variables:
REAL (KIND(0D0)), ALLOCATABLE :: da_X(:,:,:)
REAL (KIND(0D0)) :: d_Delta
REAL (KIND(0D0)) :: d_SumS
INTEGER :: i, j, k
INTEGER :: ios
!___Intrinsice Procedures:
INTRINSIC :: SUM
!. . . . . . . . . . . . . . . SUBROUTINE emrbpdval . . . . . . . . . . . . . !
!___Executable Statements:
iEr = i_OK ! no problem yet
ALLOCATE(da_X(1:i_NumGrps,1:i_NumBlks,1:i_NumVars), STAT=iEr)

IF (iEr /= i_OK) THEN
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |___________
!  |                                              |
!  | 'Memory allocation error: ia_A in EMRBPDVAL' |
!  |______________________________________________|
!
      i_CCode = iEr
      iEr = i_MRPPMOD_ER023
      RETURN   ! ... error exit ...
END IF
da_X = d_ZERO ! Initialize array.
        !?x? da_S = d_ZERO ! Initialize array.
DO i=1,i_NumGrps
   da_X(i,1,1:i_NumVars) = da_Data(i,1,1:i_NumVars)
END DO
DO i=1,i_NumGrps
   da_X(i,2,1:i_NumVars) = da_Data(ia_P2(i),2,1:i_NumVars)
END DO
IF (i_NumBlks > 2) THEN ! numb blocks > 2
   DO i=1,i_NumGrps
      da_X(i,3,1:i_NumVars) = da_Data(ia_P3(i),3,1:i_NumVars)
   END DO
   IF (i_NumBlks > 3) THEN ! numb blocks > 3
      DO i=1,i_NumGrps
         da_X(i,4,1:i_NumVars) = da_Data(ia_P4(i),4,1:i_NumVars)
      END DO
      IF (i_NumBlks > 4) THEN ! numb blocks > 4
         DO i=1,i_NumGrps
            da_X(i,5,1:i_NumVars) = da_Data(ia_P5(i),5,1:i_NumVars)
         END DO
         IF (i_NumBlks > 5) THEN ! numb blocks > 5
            DO i=1,i_NumGrps
               da_X(i,6,1:i_NumVars) = da_Data(ia_P6(i),6,1:i_NumVars)
            END DO
            IF (i_NumBlks > 6) THEN ! numb blocks > 6
               DO i=1,i_NumGrps
                  da_X(i,7,1:i_NumVars) = da_Data(ia_P7(i),7,1:i_NumVars)
               END DO
               IF (i_NumBlks > 7) THEN ! numb blocks > 7
                  DO i=1,i_NumGrps
                     da_X(i,8,1:i_NumVars) = da_Data(ia_P8(i),8,1:i_NumVars)
                  END DO
                  IF (i_NumBlks > 8) THEN ! numb blocks > 8
                     DO i=1,i_NumGrps
                        da_X(i,9,1:i_NumVars)=da_Data(ia_P9(i),9,1:i_NumVars)
                     END DO
                  END IF ! end numb blocks > 8
               END IF ! end numb blocks > 7
            END IF ! end numb blocks > 6
         END IF ! end numb blocks > 5
      END IF ! end numb blocks > 4
   END IF ! end numb blocks > 3
END IF ! end numb blocks > 2
d_Delta = d_ZERO
DO i=1,i_NumGrps
   DO j=1,i_NumBlks-1
      DO k=j+1,i_NumBlks

         d_SumS = SUM((da_X(i,j,1:i_NumVars)-da_X(i,k,1:i_NumVars))**2)
         d_Delta = d_Delta + d_SumS**(d_V/d_TWO)
         call rchkusr()
      END DO
   END DO
END DO
d_Delta = d_Delta*d_C
        ! i_KountB is # times expected < observed MRBP statistic
IF (d_Delta < d_Delta1) i_KountB = i_KountB + 1

DEALLOCATE(da_X, STAT=ios)
RETURN   ! ... normal exit ...
END SUBROUTINE emrbpdval

!=============================================================================!

SUBROUTINE emvptmp(d_V, i_NumBlocks, i_NumVars, i_NumSamples, i_LZ, i_IC,     &
      da_U_MeanV, da_Raw, i_Kount1, i_Kount2, d_Delta1, d_PValue, iEr)
           !* A NAME="emvptmp">
!___Imported Parameters and Variables:
USE blcmnmod, ONLY: d_ONE, d_TWO, d_ZERO, i_NOT_OK, i_OK, i_ZERO
        ! ^-- Some global Blossom variables and parameters.
!___Imported Procedures:
USE invertmod, ONLY: invert
        ! ^-- invert: matrix inversion routine.
IMPLICIT NONE
!___Dummy Arguments:
REAL (KIND(0D0)), INTENT(IN)    :: d_V
INTEGER,          INTENT(IN)    :: i_NumBlocks
INTEGER,          INTENT(IN)    :: i_NumVars
INTEGER,          INTENT(IN)    :: i_NumSamples
INTEGER,          INTENT(INOUT) :: i_LZ
INTEGER,          INTENT(INOUT) :: i_IC
REAL (KIND(0D0)), INTENT(INOUT) :: da_U_MeanV(:)
REAL (KIND(0D0)), INTENT(INOUT) :: da_Raw(:,:,:)
INTEGER,          INTENT(OUT)   :: i_Kount1
INTEGER,          INTENT(OUT)   :: i_Kount2
REAL (KIND(0D0)), INTENT(OUT)   :: d_Delta1
REAL (KIND(0D0)), INTENT(OUT)   :: d_PValue
INTEGER,          INTENT(OUT)   :: iEr
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     SUBROUTINE emvptmp
!  DESCRIPTION:
!    One-Sample Option: (i_NumSamples = 1)
!    This fortran program computes the test statistic and associated
!    exact P-Value for a one-sample multivariate (permutation) test with
!    hypothesized mean vector of the R responses.  The maximum values
!    of B and R can be changed for any example.  The  present maximum
!    values of B and R in this program are respectively 20 and 30.  This
!    program can invoke a Hotelling type commensuration (i_LZ = 1 & B > R).
!    This program can also invoke Average Pair-wise Distance Function
!    (D.F.) Commensuration (i_IC = 1).

! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!___Local Variables:
REAL (KIND(0D0)), ALLOCATABLE :: da_DT(:,:)
REAL (KIND(0D0)), ALLOCATABLE :: da_W(:,:)
REAL (KIND(0D0)), ALLOCATABLE :: da_Y(:,:)
REAL (KIND(0D0)), ALLOCATABLE :: da_AD(:)
REAL (KIND(0D0)), ALLOCATABLE :: da_Diff(:)
REAL (KIND(0D0)), ALLOCATABLE :: da_X(:)
INTEGER :: i, j, J1, J2, k
INTEGER :: ios
!___Intrinsic Procedures:
INTRINSIC :: ABS, SQRT
!___Executable Statements:
iEr = i_OK ! no problem yet
ALLOCATE(                                  &
        da_AD(1:i_NumVars),                &
      da_Diff(1:i_NumVars),                &
        da_DT(1:i_NumBlocks, 1:i_NumVars), &
         da_W(1:i_NumVars,   1:i_NumVars), &
         da_X(1:i_NumVars),                &
         da_Y(1:i_NumVars,   1:i_NumVars), &
      STAT=ios)
IF (ios /= i_OK) THEN
   iEr = i_NOT_OK
   RETURN   ! ... error exit ...
END IF
IF (i_NumBlocks <= i_NumVars) i_LZ = 0
IF (i_NumVars == 1) THEN
   i_LZ = 0
   i_IC = 0
END IF
IF (i_NumSamples == 2) GOTO 20
IF (i_LZ == 1) THEN
   i_IC = 0
   GOTO 10
END IF
DO j=1,i_NumBlocks
   DO k=1,i_NumVars
      da_Diff(k) = da_Raw(1,j,k) - da_U_MeanV(k)
      call rchkusr()
   END DO
   DO k=1,i_NumVars
      da_DT(j,k) = da_Diff(k)
      call rchkusr()
   END DO
END DO
GOTO 30
10 CONTINUE
DO j=1,i_NumBlocks
   DO k=1,i_NumVars
      da_Diff(k) = da_Raw(1,j,k) - da_U_MeanV(k)
      call rchkusr()
   END DO
   DO k=1,i_NumVars
      da_Raw(1,j,k) = da_Diff(k)
   END DO
END DO
GOTO 50
20 CONTINUE
IF (i_LZ == 1) THEN
   i_IC = 0
   GOTO 40
END IF
DO j=1,i_NumBlocks
   DO k=1,i_NumVars
      da_Diff(k) = da_Raw(1,j,k) - da_Raw(2,j,k)
      call rchkusr()
   END DO
   DO k=1,i_NumVars
      da_DT(j,k) = da_Diff(k)
   END DO
END DO
30 CONTINUE
IF (i_IC == 0) GOTO 60
        ! Average D.F. Commensuration is invoked
DO k=1,i_NumVars
   da_AD(k) = d_ZERO
   DO J1=2,i_NumBlocks
      DO J2=1,J1-1
         da_AD(k) = da_AD(k) + ((ABS(da_DT(J1,k)-da_DT(J2,k)))**d_V +         &
                                (ABS(da_DT(J1,k)+da_DT(J2,k)))**d_V)*d_TWO
      END DO
   END DO
   da_AD(k) = da_AD(k)**(d_ONE/d_V)
END DO
DO j=1,i_NumBlocks
   DO k=1,i_NumVars
      da_DT(j,k) = da_DT(j,k)/da_AD(k)
   END DO
END DO
GOTO 60
40 CONTINUE
DO j=1,i_NumBlocks
   DO k=1,i_NumVars
      da_Diff(k) = da_Raw(1,j,k) - da_Raw(2,j,k)
   END DO
   DO k=1,i_NumVars
      da_Raw(1,j,k) = da_Diff(k)
   END DO
END DO
50 CONTINUE
DO i=1,i_NumVars
   da_X(i) = d_ZERO
   DO j=1,i_NumBlocks
      da_X(i) = da_X(i) + ABS(da_Raw(1,j,i))
   END DO
   da_X(i) = da_X(i)/i_NumBlocks
END DO
DO i=1,i_NumVars
   DO j=i,i_NumVars
      da_W(i,j) = d_ZERO
      DO k=1,i_NumBlocks
         da_W(i,j) = da_W(i,j) + (ABS(da_Raw(1,k,i))-da_X(i)) *               &
                                 (ABS(da_Raw(1,k,j))-da_X(j))
                                 call rchkusr()
      END DO
      da_W(i,j) = da_W(i,j)/i_NumBlocks
      da_W(j,i) = da_W(i,j)
   END DO
END DO
CALL invert(i_NumVars, da_W, iEr)
IF (iEr /= i_OK) THEN
   DEALLOCATE(da_AD, da_Diff, da_DT, da_W, da_X, da_Y, STAT=ios)
   RETURN   ! ... error exit ...
END IF
da_Y(1,1) = SQRT(da_W(1,1))
DO i=2,i_NumVars
   da_Y(1,i) = da_W(1,i)/da_Y(1,1)
   da_Y(i,1) = d_ZERO
END DO
DO i=2,i_NumVars
   da_Y(i,i) = da_W(i,i)
   DO j=1,i-1
      da_Y(i,i) = da_Y(i,i) - da_Y(j,i)*da_Y(j,i)
      call rchkusr()
   END DO
   da_Y(i,i) = SQRT(da_Y(i,i))
   DO j=i+1,i_NumVars
      da_Y(i,j) = da_W(i,j)
      da_Y(j,i) = d_ZERO
      DO k=1,i-1
         da_Y(i,j) = da_Y(i,j) - da_Y(k,i)*da_Y(k,j)
         call rchkusr()
      END DO
      da_Y(i,j) = da_Y(i,j)/da_Y(i,i)
   END DO
END DO
DO i=1,i_NumBlocks
   DO j=1,i_NumVars
      da_DT(i,j) = d_ZERO
      DO k=j,i_NumVars
         da_DT(i,j) = da_DT(i,j) + da_Y(j,k)*da_Raw(1,i,k)
         call rchkusr()
      END DO
   END DO
END DO
60 CONTINUE
CALL emvptmpCOMB(d_V, i_NumBlocks, i_NumVars, da_DT, d_Delta1, i_Kount1,      &
      i_Kount2, d_PValue, iEr)
DEALLOCATE(da_AD, da_Diff, da_DT, da_W, da_X, da_Y, STAT=ios)
RETURN   ! ... normal and error exits here ...
END SUBROUTINE emvptmp

!=============================================================================!

SUBROUTINE emvptmpCOMB(d_V, i_NumBlocks, i_NumVars, da_DT, d_Delta1,          &
      i_Kount1, i_Kount2, d_PValue, iEr)
!___Imported Parameters and Variables:
USE blcmnmod, ONLY: i_NOT_OK, i_OK
        ! ^-- Some global Blossom variables and parameters.
!___Imported Procedures:
!__(none)__
IMPLICIT NONE
!___Dummy Arguments:
REAL (KIND(0D0)), INTENT(IN)  :: d_V
INTEGER,          INTENT(IN)  :: i_NumBlocks
INTEGER,          INTENT(IN)  :: i_NumVars
REAL (KIND(0D0)), INTENT(IN)  :: da_DT(:,:)
REAL (KIND(0D0)), INTENT(OUT) :: d_Delta1
INTEGER,          INTENT(OUT) :: i_Kount1
INTEGER,          INTENT(OUT) :: i_Kount2
REAL (KIND(0D0)), INTENT(OUT) :: d_PValue
INTEGER,          INTENT(OUT) :: iEr
!___Local Variables:
INTEGER, ALLOCATABLE :: ia_M(:)
INTEGER :: j
INTEGER :: ios
INTEGER :: i_Kount3, i_NMinus, i_Last
!___Intrinsic Procedures:
INTRINSIC :: DBLE, MOD
!___Executable Statements:
iEr = i_OK ! no problem yet
ALLOCATE(ia_M(1:i_NumBlocks), STAT=ios)
IF (ios /= i_OK) THEN
   iEr = i_NOT_OK
   RETURN   ! ... error exit ...
END IF
ia_M     = 1
i_Kount1 = 0
i_Kount2 = 0
i_Kount3 = 0
i_NMinus = 0
i_Last   = 0
GOTO 50
20 CONTINUE
j = 0
        ! 30    CONTINUE
loop30: DO
   j = j + 1
        !    IF (MOD(i_Kount3,2) == 1) GOTO 40
   IF (MOD(i_Kount3, 2) == 1) EXIT
   i_Kount3 = i_Kount3 / 2
        !    GOTO 30
END DO loop30
        ! 40    CONTINUE
i_Last  = ia_M(j)
ia_M(j) = -i_Last
50 CONTINUE
i_Kount1 = i_Kount1 + 1
i_Kount3 = i_Kount1
i_NMinus = i_NMinus + i_Last
CALL emvptmpXACT(d_V, i_NumBlocks, i_NumVars, i_Kount1, i_Kount2, d_Delta1,   &
      ia_M, da_DT)
IF (i_NMinus == 1) THEN
   IF (ia_M(i_NumBlocks) == -1) THEN
      GOTO 60
   END IF
END IF
GOTO 20
60 CONTINUE
d_PValue = DBLE(i_Kount2)/DBLE(i_Kount1)
DEALLOCATE(ia_M, STAT=ios)
RETURN   ! ... normal exit ...
END SUBROUTINE emvptmpCOMB

!=============================================================================!

SUBROUTINE emvptmpXACT(d_V, i_NumBlocks, i_NumVars, i_Kount1, i_Kount2,       &
      d_Delta1, ia_M, da_DT)
!___Imported Parameters and Variables:
USE blcmnmod, ONLY: d_ONE, d_TWO, d_ZERO
        ! ^-- Some global Blossom variables and parameters.
!___Imported Procedures:
!__(none)__
IMPLICIT NONE
!___Dummy Arguments:
REAL (KIND(0D0)), INTENT(IN)  :: d_V
INTEGER,          INTENT(IN)  :: i_NumBlocks
INTEGER,          INTENT(IN)  :: i_NumVars
INTEGER,          INTENT(IN)  :: i_Kount1
INTEGER,          INTENT(OUT) :: i_Kount2
REAL (KIND(0D0)), INTENT(OUT) :: d_Delta1
INTEGER,          INTENT(IN)  :: ia_M(:)
REAL (KIND(0D0)), INTENT(IN)  :: da_DT(:,:)
!___Procedure Parameters:
REAL (KIND(0D0)), PARAMETER :: d_PRECISION = 1.0D-12
!___Local Variables:
REAL (KIND(0D0)) :: d_B, d_Sum, d_PSum, d_Delta
INTEGER :: i, j, k
!___Intrinsic Procedures:
INTRINSIC :: DBLE
!___Executable Statements:
d_B = DBLE(i_NumBlocks)
d_Sum = d_ZERO
DO i=1,i_NumBlocks-1
   DO j=i+1,i_NumBlocks
      d_PSum = d_ZERO
      DO k=1,i_NumVars
         d_PSum = d_PSum + (da_DT(i,k)*DBLE(ia_M(i))-da_DT(j,k)*DBLE(ia_M(j)))**2
      END DO
      d_Sum = d_Sum + d_PSum**(d_V/d_TWO)
   END DO
END DO
d_Delta = d_Sum*d_TWO/(d_B*(d_B-d_ONE))
IF (i_Kount1 == 1) d_Delta1 = d_Delta*(d_ONE+d_PRECISION)
IF (d_Delta <= d_Delta1) i_Kount2 = i_Kount2 + 1
RETURN   ! ... normal exit ...
END SUBROUTINE emvptmpXACT

!=============================================================================!


SUBROUTINE commenspp(i_NumObs,   &
                     i_NumVars,  &
                     d_DistExp,  &
                     da_Data,    &
                     da_AvgDist, &
                     iEr)
!___Imported Parameters:
USE blcmnmod, ONLY: ch_NL, d_ONE, d_TWO, d_ZERO, i_NOT_OK, i_OK
        ! ^-- Some global Blossom variables and parameters.
USE jonsmodule, ONLY: i_cmsramod_ER001
        ! ^-- Message handling, etc.
!___Imported Procedures:
!__(none)__
IMPLICIT NONE
!___Dummy Aruguments:
INTEGER,          INTENT(IN)    :: i_NumObs
INTEGER,          INTENT(IN)    :: i_NumVars
REAL (KIND(0D0)), INTENT(IN)    :: d_DistExp
REAL (KIND(0D0)), INTENT(INOUT) :: da_Data(:,:)
        !  dim(1:i_NumObs,1:i_NumVars+1) --^
REAL (KIND(0D0)), INTENT(OUT)   :: da_AvgDist(:)
        !                dim(i_NumVars)--^
INTEGER,          INTENT(OUT)   :: iEr
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     SUBROUTINE commenspp
! DESCRIPTION:
!     Commensurate MRPP data set.
!     Divide the data values by the "average distance"
!     value of each variable so that the variable values are on commensurate
!     "units" of measurement. The "average distance function" is from Mielke.
!
! LANGUAGE:
!     Fortran 90, with Lahey Computer Systems LF90 intrinsics extensions
! AUTHOR:
!     Dr. Paul W. Mielke, Jr., Dept of Statistics, Colorado State University
!        http://www.stat.colostate.edu/~mielke/mielkesoftware.html
!     Modifications:
!     Jon D. Richards, Operations Research Analyst       jon_richards@usgs.gov
!     Fort Collins Science Center                     http://www.fort.usgs.gov
!     U.S. Geological Survey          -          Biological Resources Division
! INVOKED BY:
!     runemrpp    <File: "domrpp.f90: runemrpp"> module
!     runmrpp     <File: "domrpp.f90: runmrpp">  module
!     rumrsp      <File: "domrsp.f90: runmrsp">  module
! INVOKES:
!     -

! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!___Procedure Parameters:
        ! d_TOLER is how close to zero we allow average distance to get
        ! before we decide not to commensurate, but call an error
REAL (KIND(0D0)), PARAMETER :: d_TOLER = 1.0D-11 ! How close to 0?
!___Local Variables:
REAL (KIND(0D0)) :: d_Sum
INTEGER :: i, j, k
!___Intrinsic Procedures:
INTRINSIC :: ABS, DBLE
!___Executable Statements:

iEr = i_OK ! no problem yet
IF (i_NumVars > 1) THEN  ! If multivariate, then commensurate variables.
   DO i=1,i_NumVars
      d_Sum = d_ZERO
      DO j=1,i_NumObs-1
         DO k=j+1,i_NumObs
             d_Sum = d_Sum + (ABS(da_Data(j,i)-da_Data(k,i)))**d_DistExp
             call rchkusr()
         END DO
      END DO
      d_Sum = d_Sum*d_TWO/DBLE(i_NumObs)/DBLE(i_NumObs-1) ! From Brian Cade.
      da_AvgDist(i) = d_Sum**(d_ONE/d_DistExp)
   END DO
   DO i=1,i_NumObs
      DO j=1,i_NumVars
         IF (da_AvgDist(j) < d_TOLER) THEN ! If average distance is zero (or
                                           ! very near zero), then error exit.
!   ____________________________________________________
!  |                                                    |
!  | 'Average distance for a variable is zero or'       |
!  | 'near zero.  To divide by this for commensuration' |
!  | 'would result in very large data values for the'   |
!  | 'analytical program.  This suggests the values'    |
!  | 'are very close in the data space.  Inspect your'  |
!  | 'data for errors, or for a variable with very'     |
!  | 'small variability.  You might want to discard'    |
!  | 'this variable in your analysis'                   |
!  |____________________________________________________|
!
            iEr = i_cmsramod_ER001
            RETURN   ! ... error exit ...
         ELSE
            da_Data(i,j) = da_Data(i,j) / da_AvgDist(j)
         END IF
      END DO
   END DO
END IF
RETURN   ! ... normal exit ...
END SUBROUTINE commenspp


SUBROUTINE commensbp(i_NumGrps,  &
                     i_NumBlks,  &
                     i_NumVars,  &
                     d_DistExp,  &
                     da_Data,    &
                     da_AvgDist, &
                     iEr)
!___Imported Parameters and Variables:
USE blcmnmod, ONLY: ch_NL, d_ONE, d_ZERO, i_NOT_OK, i_OK, l_TRUE
        ! ^-- Some global Blossom variables and parameters.
USE jonsmodule, ONLY: i_cmsramod_ER001
        ! ^-- Message handling, etc.
!___Imported Procedures:
!__(none)__
IMPLICIT NONE
!___Dummy Aruguments:
INTEGER,          INTENT(IN)    :: i_NumGrps
INTEGER,          INTENT(IN)    :: i_NumBlks
INTEGER,          INTENT(IN)    :: i_NumVars
REAL (KIND(0D0)), INTENT(IN)    :: d_DistExp
REAL (KIND(0D0)), INTENT(INOUT) :: da_Data(:,:,:)
        ! dim(i_NumGrps,i_NumBlks,i_NumVars)--^
REAL (KIND(0D0)), INTENT(OUT)   :: da_AvgDist(:)
        !              dim(i_NumMRVars)--^
INTEGER,          INTENT(OUT)   :: iEr
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     SUBROUTINE commensbp
! DESCRIPTION:
!     Commensurate an MRBP data set.
!     Extracted from MRBP of Dr. Paul W. Mielke, Jr. as amended 7/27/90.
! LANGUAGE:
!     Fortran 90, with Lahey Computer Systems LF90 intrinsics extensions
! AUTHOR:
!     Dr. Paul W. Mielke, Jr., Dept of Statistics, Colorado State University
!        http://www.stat.colostate.edu/~mielke/mielkesoftware.html
!     Modifications:
!     Jon D. Richards, Operations Research Analyst       jon_richards@usgs.gov
!     Fort Collins Science Center                     http://www.fort.usgs.gov
!     U.S. Geological Survey          -          Biological Resources Division
! INVOKED BY:
!     runmrbp     <File: "domrpp.f90: runmrbp"> module
! INVOKES:
!     -
! FILES:
!     -

! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!___Procedure Parameters:
        ! d_TOLER is how close to zero we allow average distance to get
        ! before we decide not to commensurate, but call an error
REAL (KIND(0D0)), PARAMETER :: d_TOLER = 1.0D-11 ! How close to zero?
!___Local Variables:
REAL (KIND(0D0)) :: d_Sum
INTEGER :: i, j, k, I1, I2, J1, J2
!___Intrinsic Procedures:
INTRINSIC :: ABS, DBLE
!___Executable Statements:
iEr = i_OK ! no problem yet
DO k=1,i_NumVars           ! commensuration
   d_Sum = d_ZERO
   DO I1=1,i_NumGrps
      DO J1=1,i_NumBlks
         DO I2=1,i_NumGrps
            DO J2=1,i_NumBlks
               d_Sum = d_Sum + &
                     (ABS(da_Data(I1,J1,k)-da_Data(I2,J2,k)))**d_DistExp
                     call rchkusr()
            END DO
         END DO
      END DO
   END DO
   d_Sum = d_Sum / DBLE((i_NumBlks*i_NumGrps)*((i_NumBlks*i_NumGrps)-1))
   da_AvgDist(k) = d_Sum**(d_ONE/d_DistExp)
END DO
DO i=1,i_NumGrps
   DO j=1,i_NumBlks
      DO k=1,i_NumVars
         IF (da_AvgDist(k) < d_TOLER) THEN
!   ____________________________________________________
!  |                                                    |
!  | 'Average distance for a variable is zero or'       |
!  | 'near zero.  To divide by this for commensuration' |
!  | 'would result in very large data values for the'   |
!  | 'analytical program.  This suggests the values'    |
!  | 'are very close in the data space.  Inspect your'  |
!  | 'data for errors, or for a variable with very'     |
!  | 'small variability.  You might want to discard'    |
!  | 'this variable in your analysis'                   |
!  |____________________________________________________|
!
            iEr = i_cmsramod_ER001
            RETURN   ! ... error exit ...
         ELSE
            da_Data(i,j,k) = da_Data(i,j,k) / da_AvgDist(k)
         END IF
         call rchkusr()
      END DO
   END DO
END DO
RETURN   ! ... normal exit ...
END SUBROUTINE commensbp

SUBROUTINE runmrpp(da_Data, iEr, i_NumObs, i_NumVars, d_V, d_Sup, i_CForm, l_DoHot,l_IsCommens, i_NumGrps,    &
      d_NInterv, ia_GrpSizes, i_NumPerms, l_DoResample, da_XI,     &
      d_T, d_DBar, d_D1, d_Var, d_Gam, d_PValue, da_Hot, l_HasExcess,       &
      d_ExcessVal, da_GpVals, i_ActiveCases, da_GroupV,ia_GrpValTag,  &
      i_Seed,l_SaveTest,da_STV,da_CommAvgDist)


!USE mrppmod !might need to list all modules called by mrpp
USE blcmnmod, ONLY: i_OK
USE jonsmodule
!USE mrppmod
              ! debug!
USE blcmnmod, ONLY: cha_CmdStack, ch_BLANK, ch_NL, d_ZERO,    &
      i_NOT_OK, i_NumCases, i_OK, l_CLEAR, l_Terse
        ! ^-- Some global Blossom variables and parameters.
USE ioformod, ONLY: ch25_OutBuff, ch12_OutBuff, ch12_OutBuff2
        ! ^-- Output buffers for character output of numbers.
USE jonsmodule, ONLY: ch_EM00033, ch_EM00047, ch_EM00154, ch_EM00155,         &
      ch_EM00156, ch_EM00157, ch_EM00158, ch_EM00159, ch_EM00192, ch_STR0013, &
      ch_MAT_SINGULARITY,                                                     &
      ch_MRPPMOD_EM001, ch_MRPPMOD_EM002, ch_MRPPMOD_EM003, ch_MRPPMOD_EM026, &
      ch_MRPPMOD_EM027, ch_MRPPMOD_EM028, ch_MRPPMOD_EM029,                   &
      i_CCode,                                                                &
      i_MRPPMOD_ER001, i_MRPPMOD_ER002, i_MRPPMOD_ER003, i_MRPPMOD_ER026,     &
      i_MRPPMOD_ER027, i_MRPPMOD_ER028, i_MRPPMOD_ER029
        ! ^-- Message handling, etc.
USE mrolamod, ONLY: d_ArcIntv, d_DEFAULT_NO_ARC, d_DEFLT_NO_TRUNC, d_Trunc,   &
      i_MAT_SINGULARITY, i_MIN_GRP_SIZE_MRPP, i_MIN_MRPP_OBS,                 &
      i_MIN_NUM_GPS_MRPP, i_NumGVVals, i_NumGps,   &
      i_NumMVs, i_NumRcd, i_USE_SYS_RAND, i_XS_FLAG, &
      l_HasGVals
        ! ^-- Permutation, regression and other Blossom stat parameters.
!___Imported Procedures:

USE jonsmodule, ONLY: errhand, getrseed

USE mt19937, ONLY: init_genrand
        ! ^-- Mersenne Twister Pseudo Random Number Generator
IMPLICIT NONE
!___Dummy Arguments:

REAL (KIND(0D0)), INTENT (INOUT)   :: da_Data(:,:)
LOGICAL,          INTENT(IN)    :: l_HasExcess
REAL (KIND(0D0)), INTENT(INOUT) :: d_ExcessVal
REAL (KIND(0D0)), INTENT(INOUT) :: da_GpVals(:)   ! dim(i_NumCases)
INTEGER,          INTENT(IN)   :: i_ActiveCases
REAL (KIND(0D0)), INTENT(IN)    :: da_GroupV(:)   ! dim(i_NumCases)
INTEGER,          INTENT(OUT)   :: iEr
REAL (KIND(0D0)), INTENT(OUT)   ::d_PValue
REAL (KIND(0D0)), INTENT(OUT) :: da_XI(:)
REAL (KIND(0D0)), INTENT(OUT) :: da_Hot(:,:)
REAL (KIND(0D0)), INTENT(IN) :: d_NInterv
INTEGER,  		  INTENT(IN) :: i_NumVars
INTEGER,	  	  INTENT(IN) :: i_CForm
LOGICAL,	  	  INTENT(INOUT) :: l_DoHot
LOGICAL,	  	  INTENT(INOUT) :: l_IsCommens
INTEGER,	  	  INTENT(IN) :: i_NumPerms
INTEGER,	  	  INTENT(IN) :: i_NumObs
INTEGER,	  	  INTENT(IN) :: i_NumGrps
REAL (KIND(0D0)), INTENT(IN) :: d_V
REAL (KIND(0D0)), INTENT(IN) :: d_Sup
REAL (KIND(0D0)), INTENT(OUT):: d_T
REAL (KIND(0D0)), INTENT(OUT):: d_DBar
REAL (KIND(0D0)), INTENT(OUT):: d_D1
REAL (KIND(0D0)), INTENT(OUT):: d_Var
REAL (KIND(0D0)), INTENT(OUT):: d_Gam
!REAL (KIND(0D0)), INTENT(OUT):: d_Delta1
LOGICAL, 		  INTENT(IN) :: l_DoResample !, file_exists
INTEGER,		  INTENT(IN) :: ia_GrpSizes(:)
INTEGER, 		 INTENT(IN) :: ia_GrpValTag(i_NumObs)  ! dim(i_NumCases)
INTEGER,	  	  INTENT(INOUT) :: i_Seed
LOGICAL, 		  INTENT(IN) :: l_SaveTest
REAL (KIND(0D0)), INTENT(OUT) :: da_STV(:)
REAL (KIND(0D0)), INTENT(OUT) :: da_CommAvgDist(:)
                            ! debug!
!___Imported Parameters and Variables:
!USE bfilemod, ONLY: ch_HoldPathFile, ch_TempPathFile, i_NumCmdGpVarVals,      &
!      i_NumHoldCases, i_TempRecLen, i_TEMP_UNIT, l_HasHoldFile, l_SAVETEST
        ! ^-- Misc. Blossom file names, units, status parameters.
!d USE bfilemod, ONLY: l_HasTempFile
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     SUBROUTINE runmrpp
! DESCRIPTION:
!     Run a Multi-Response Permutation Procedure (MRPP).
!     -Tag active grouping variable values.
!     -Make sure we have enough data to do MRPP.
!     -Call the MRPP routine to compute the MRPP stats and then display
!      results with DISPMRPP.

! INVOKED BY:
!     runmrprocs      <File: "domrpp.f90: runmrprocs">        module
! INVOKES:
!     chkmrpp         <File: "domrpp.f90: chkmrpp">           module
!     commenspp       <File: "cmsramod.f90: commenspp">       module
!     delfil          <File: "jonsmodule.f90: delfil">        module
!     dispmrpp        <File: "domrpp.f90: dispmrpp">          module
!     emitstr         <File: "jonsmodule.f90: emitstr">       module
!     errhand         <File: "jonsmodule.f90: errhand">       module
!     getactivevarlst <File: "mrauxmod.f90: getactivevarlst"> module
!     getrseed        <File: "jonsmodule.f90: getrseed">      module
!     mrpp            <File: "mrppmod.f90: mrpp">             module
!     renfil          <File: "jonsmodule.f90: renfil">        module
!     setgpvaltags    <File: "mrauxmod.f90: setgpvaltags">    module
! FILES:
!     TEMP-DAT.TMP <TEMP file> OPEN (DIRECT) READ CLOSE

!___Procedure Parameters:
CHARACTER (LEN=*), PARAMETER ::  ch_PROB_MSG = "Problem reading TEMP file"
CHARACTER (LEN=*), PARAMETER :: ch_PROC_NAME = "RUNMRPP"
!___Local Variables:


REAL (KIND(0D0)), ALLOCATABLE :: da_Values(:)

REAL (KIND(0D0)) :: d_NumObs
REAL (KIND(0D0)) :: d_NumGrps

INTEGER :: i, k, j
INTEGER :: ios
INTEGER :: i_Exc
INTEGER :: i_LZ
INTEGER :: i_NumXSRecs
CHARACTER (LEN=4) :: blcmd
INTEGER, PARAMETER :: i_LEN_MY_EMSG = 254 ! needs to be this big for output of var list in dostatus.
CHARACTER (LEN=i_LEN_MY_EMSG) :: ch_MyMsg
INTEGER :: i_count
! these are local variables that I'm declaring they might cause problems, I'm nos sure where they  come from
INTEGER :: i_numcmdgpvarvals
INTEGER :: i_numholdcases
INTEGER :: i_temp_unit
INTEGER, ALLOCATABLE :: ia_ActiveVarNdx(:)


INTRINSIC :: ALLOCATED, DBLE, RANDOM_SEED, SIZE


d_NumObs=i_NumObs
d_NumGrps=i_NumGrps
iEr = i_OK ! no problem yet

ALLOCATE(                         &
         da_Values(1:i_NumObs),  &
      STAT=ios)

IF (ios /= i_OK) THEN

!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |___________________________
!  |                                                              |
!  | 'Memory allocation error: da_Values/ia_GrpValTag in RUNMRPP' |
!  |______________________________________________________________|
!
   CALL errhand(ch_myMsg, i_Code=ios,        &
                ch_Msg=ch_EM00033, &
                 ch_S1="da_Values,ia_GrpValTag", &
                 ch_S2=ch_PROC_NAME)
   iEr = i_NOT_OK

   GOTO 7000
END IF

ALLOCATE(                         &
         ia_ActiveVarNdx(1:i_NumVars),  &
      STAT=ios)

ia_ActiveVarNdx = 0  ! zero out array
i_Count = 0

   DO i=1,i_NumVars   ! Number of variables in command line.
         i_Count = i_Count + 1
         ia_ActiveVarNdx(i_Count) = i
   END DO

call rchkusr()
IF (l_IsCommens) THEN  ! If Hotelling's commensuration ...
   IF (l_DoHot) THEN
      l_IsCommens = .FALSE.  ! ... then NOT euclidean commensuration.
   END IF
END IF
! Tag the active grouping values and excess grouping values.

call rchkusr()

! If Hotelling's commensuration specified, we must have
! number cases > number variables and multivariate.


IF (l_DoHot) THEN
   IF (i_ActiveCases < i_NumVars) THEN
!   _________________________________________________________________________
!  |                                                                         |
!  | 'For Hotelling commensuration the number of cases must be greater than' |
!  | 'the number of variables'                                               |
!  |_________________________________________________________________________|
!
      call errhand(ch_myMsg, ch_Msg=ch_EM00157)
      l_DoHot = .FALSE.
      iEr = i_NOT_OK
      GOTO 7000
   END IF
   IF (i_NumVars == 1) THEN
!   ___________________________________________________________________
!  |                                                                   |
!  | 'You can only do Hotelling commensuration with multivariate data' |
!  |___________________________________________________________________|
!
      call errhand(ch_myMsg, ch_Msg=ch_EM00158)
      l_DoHot = .FALSE.
      iEr = i_NOT_OK
      GOTO 7000
   END IF
END IF
! We know the num active cases, num vars, distance exponent,
! distance upper bound=0, c(I), Hotelling's commensuration choice,
! num groups.

IF (l_DoHot) THEN
   i_LZ = 1  ! Hotelling commensuration to be done.
ELSE
   i_LZ = 0
END IF


IF (l_HasExcess) THEN !
	i_NumXSRecs=ia_GrpSizes(i_NumGrps+1)
END IF

                                                             ! debug!
! Commensurate the data if we need to:
IF (l_IsCommens) THEN
   IF (.NOT. l_DoHot) THEN
      CALL commenspp(i_ActiveCases,  & !* <File: "cmsramod.f90: commenspp">
                     i_NumVars,    &
                     d_V,    &
                     da_Data,        &
                     da_CommAvgDist, &
                     iEr)
      IF (iEr /= i_OK) THEN
         iEr = i_NOT_OK

         GOTO 7000
      END IF
   END IF
END IF

call rchkusr()
! Run the MRPP procedure:
CALL mrpp(DBLE(i_ActiveCases), i_NumVars, d_V, d_sup, i_CForm,   &
      i_LZ, DBLE(i_numGrps), d_NInterv, ia_GrpSizes, i_NumPerms,              &
      l_DoResample, da_Data, da_XI, d_T, d_DBar, d_D1, d_Var, d_Gam, d_PValue,    &
      da_Hot, iEr,i_Seed,l_SaveTest,da_STV) !* <File: "mrppmod.f90: mrpp">

IF (iEr /= i_OK) THEN      ! BEGIN Error handling
call rchkusr()
   SELECT CASE (iEr)  ! Error handling
      CASE (i_MRPPMOD_ER001)
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |____________
!  |                                               |
!  | 'Memory allocation error: daYHotHold in MRPP' |
!  |_______________________________________________|
!
         call errhand(ch_myMsg, i_Code=i_CCode, &
                      ch_Msg=ch_MRPPMOD_EM001)

      CASE (i_MRPPMOD_ER002)
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |_____________________________
!  |                                                                |
!  | 'Memory allocation error: da_C,da_D,da_DSub1,da_DSub2 in MRPP' |
!  |________________________________________________________________|
!
         call errhand(ch_myMsg, i_Code=i_CCode, &
                      ch_Msg=ch_MRPPMOD_EM002)
      CASE (i_MRPPMOD_ER003)
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |_____________
!  |                                                |
!  | 'Memory allocation error: da_UHot in MRPPDIST' |
!  |________________________________________________|
!
         call errhand(ch_myMsg, i_Code=i_CCode, &
                      ch_Msg=ch_MRPPMOD_EM003)
      CASE (i_MRPPMOD_ER026)
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |___________
!  |                                              |
!  | 'Memory allocation error: da_XI in MRPPDIST' |
!  |______________________________________________|
!
         call errhand(ch_myMsg, i_Code=i_CCode, &
                      ch_Msg=ch_MRPPMOD_EM026)
      CASE (i_MRPPMOD_ER027)
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |_________________
!  |                                                    |
!  | 'Memory allocation error: da_DataHold in MRPPDIST' |
!  |____________________________________________________|
!
         call errhand(ch_myMsg, i_Code=i_CCode, &
                      ch_Msg=ch_MRPPMOD_EM027)
      CASE (i_MRPPMOD_ER028)
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |______
!  |                                         |
!  | 'Memory allocation error: da_D in MRPP' |
!  |_________________________________________|
!
         call errhand(ch_myMsg, i_Code=i_CCode, &
                      ch_Msg=ch_MRPPMOD_EM028)
      CASE (i_MRPPMOD_ER029)
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |____________
!  |                                               |
!  | 'Memory allocation error: da_DPacked in MRPP' |
!  |_______________________________________________|
!
         call errhand(ch_myMsg, i_Code=i_CCode, &
                      ch_Msg=ch_MRPPMOD_EM029)
      CASE (i_MAT_SINGULARITY)
!   _____________________________________________________
!  |                                                     |
!  | 'Either all the elements of some row were zero or'  |
!  | 'a pivot became relatively zero.'                   |
!  | 'The variance/covariance matrix of variable values' |
!  | 'is probably singular and cannot be inverted.'      |
!  | 'Hotelling commensuration cannot be done.'          |
!  |_____________________________________________________|
!
         call errhand(ch_myMsg, i_Code=i_CCode, &
                      ch_Msg=ch_MAT_SINGULARITY)
      CASE DEFAULT
   END SELECT
   iEr = i_NOT_OK
END IF                     ! END Error handling

7000 CONTINUE

IF (ALLOCATED(ia_ActiveVarNdx)  ) DEALLOCATE(ia_ActiveVarNdx,    STAT=ios)
DEALLOCATE(da_Values, STAT=ios)
 IF (iEr /= i_OK) THEN
   call rexit(ch_MyMsg)
END IF

RETURN   ! ... normal exit ...
END SUBROUTINE runmrpp


SUBROUTINE runemrpp(da_Data,ia_GrpSizes, l_HasExcess,d_ExcessVal,l_DoHot,l_IsCommens,d_Trunc,d_ArcIntv,      &
      d_V_DistExp, i_CValue, i_NumGroups,           &
      i_NumMRVars, da_GpVals, i_ActiveCases, da_GroupV, d_Delta1, da_YHot,      &
      d_PValue,iEr,da_CommAvgDist)

!___Imported Parameters and Variables:

USE blcmnmod, ONLY: cha_CmdStack, ch_BLANK, ch_NL, ch_Output_Title, d_ZERO,   &
      ia_ActiveVarNdx, i_NOT_OK, i_NumCases, i_NumVars, i_OK, l_CLEAR, l_Terse
        ! ^-- Some global Blossom variables and parameters.
USE ioformod, ONLY: ch25_OutBuff, ch12_OutBuff, ch12_OutBuff2
        ! ^-- Output buffers of character format for numeric data.
USE jonsmodule, ONLY: ch_EM00033, ch_EM00047, ch_EM00157, ch_EM00158,         &
      ch_EM00160, ch_EM00161, ch_EM00162, ch_EM00163, ch_EM00192,             &
      ch_MAT_SINGULARITY, ch_MRPPMOD_EM004, ch_MRPPMOD_EM005,                 &
      ch_MRPPMOD_EM006, ch_MRPPMOD_EM007, ch_STR0013, ch_STR0013, i_CCode,    &
      i_MRPPMOD_ER004, i_MRPPMOD_ER005, i_MRPPMOD_ER006, i_MRPPMOD_ER007
        ! ^-- Message handling, etc.
USE mrolamod, ONLY:  d_DEFAULT_NO_ARC, d_DEFLT_NO_TRUNC,    &
      i_MAT_SINGULARITY, i_MIN_EMRPP_OBS, i_MIN_GRP_SIZE_EMRPP,               &
      i_MIN_NUM_GPS_EMRPP, i_NumGVVals, i_NumPerms => i_NumPermut, i_NumGps,  &
      i_NumMVs, i_NumRcd, i_Seed => i_RandNumSeed, i_XS_FLAG,       &
      l_DoResample, l_HasGVals
        ! ^-- Permutation, regression and other Blossom stat parameters.
!___Imported Procedures:
USE jonsmodule, ONLY: errhand
        ! ^-- delfil: Delete a file.
        ! ^-- emitstr: Display a string to the console output stream.
        

IMPLICIT NONE
!___Dummy Arguments:
REAL (KIND(0D0)), INTENT (INOUT) :: da_Data(:,:)
INTEGER,          INTENT(INOUT) :: ia_GrpSizes(:)   ! dim(i_NumCases)
LOGICAL,          INTENT(IN)    :: l_HasExcess
LOGICAL,          INTENT(INOUT) :: l_DoHot
LOGICAL,          INTENT(INOUT)    :: l_IsCommens
REAL (KIND(0D0)), INTENT(INOUT) :: d_ExcessVal
REAL (KIND(0D0)), INTENT(IN)    :: d_V_DistExp
INTEGER,          INTENT(IN)    :: i_CValue
INTEGER,          INTENT(INOUT) :: i_NumGroups
INTEGER,          INTENT(IN)    :: i_NumMRVars
REAL (KIND(0D0)), INTENT(INOUT) :: da_GpVals(:)   ! dim(i_NumCases)
INTEGER,          INTENT(OUT)   :: i_ActiveCases
REAL (KIND(0D0)), INTENT(IN)    :: da_GroupV(:)   ! dim(i_NumCases)
INTEGER,          INTENT(OUT)   :: iEr
REAL (KIND(0D0)), INTENT(OUT)    :: da_YHot(:,:)
REAL (KIND(0D0)), INTENT(OUT) :: d_Delta1
REAL (KIND(0D0)), INTENT(OUT)    :: d_PValue
REAL (KIND(0D0)), INTENT(IN)    :: d_Trunc
REAL (KIND(0D0)), INTENT(IN)    :: d_ArcIntv
REAL (KIND(0D0)), INTENT(OUT) :: da_CommAvgDist(:)

! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     SUBROUTINE runemrpp
! DESCRIPTION:
!     Set up, check, run Exact MRPP and then display the results.
!     - JDR  31 Mar 2000
!     This is similar to RUNMRPP.
!     Obsolete:
!        Build an EMRPP data set file (ch_EMRPP_Bd_FILE) named 'emrpp.$bd'.
! LANGUAGE:
!     Fortran 90/95
! AUTHOR:
!     Jon D. Richards, Operations Research Analyst       jon_richards@usgs.gov
!     Fort Collins Science Center                     http://www.fort.usgs.gov
!     U.S. Geological Survey          -          Biological Resources Division
! INVOKED BY:
!     runmrprocs      <File: "domrpp.f90: runmrprocs">        module
! INVOKES:
!     chkmrpp         <File: "domrpp.f90: chkmrpp">           module
!     commenspp       <File: "cmsramod.f90: commenspp">       module
!     delfil          <File: "jonsmodule.f90: delfil">        module
!     dispmrpp        <File: "domrpp.f90: dispmrpp">          module
!     emitstr         <File: "jonsmodule.f90: emitstr">       module
!     emrpp           <File: "mrppmod.f90: emrpp">            module
!     errhand         <File: "jonsmodule.f90: errhand">       module
!     getactivevarlst <File: "mrauxmod.f90: getactivevarlst"> module
!     renfil          <File: "jonsmodule.f90: renfil">        module
!     setgpvaltags    <File: "mrauxmod.f90: setgpvaltags">    module
! FILES:
!     TEMP-DAT.TMP <TEMP file> OPEN READ CLOSE
! DEVICES:
!     -

! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!___Procedure Parameters:
CHARACTER (LEN=*), PARAMETER ::  ch_PROB_MSG = "Problem reading TEMP file"
CHARACTER (LEN=*), PARAMETER :: ch_PROC_NAME = "RUNEMRPP"
!___Local Variables:


REAL (KIND(0D0)), ALLOCATABLE :: da_XI(:)
REAL (KIND(0D0)), ALLOCATABLE :: da_Values(:)
REAL (KIND(0D0)) :: d_Dum
INTEGER, ALLOCATABLE :: ia_GrpValTag(:)  ! dim(i_NumCases)
INTEGER :: i, j, k
INTEGER :: ios
INTEGER :: i_Exc
INTEGER :: i_LZ, i_NumXSRecs
CHARACTER (LEN=4) :: blcmd
INTEGER, PARAMETER :: i_LEN_MY_EMSG = 254 ! needs to be this big for output of var list in dostatus.
CHARACTER (LEN=i_LEN_MY_EMSG) :: ch_MyMsg

!___Intrinsic Procedures:
INTRINSIC :: ALLOCATED

iEr = i_OK ! no problem yet
ALLOCATE(                         &
         da_Values(1:i_NumVars),  &
      ia_GrpValTag(1:i_NumCases), &
      STAT=ios)
IF (ios /= i_OK) THEN
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |____________________________
!  |                                                               |
!  | 'Memory allocation error: da_Values,ia_GrpValTag in RUNEMRPP' |
!  |_______________________________________________________________|
!
   call errhand(ch_myMsg,i_Code=ios,        & !* <File: "jonsmodule.f90: errhand">
                ch_Msg=ch_EM00033, &
                 ch_S1="da_Values,ia_GrpValTag", &
                 ch_S2=ch_PROC_NAME)
        iEr = i_NOT_OK
   GOTO 7000
END IF
IF (l_DoHot) THEN  ! If Hotelling's commensuration, ...
   l_IsCommens = .FALSE. ! ... then we won't do avg dist commens.
END IF



! Tag the active grouping values and excess grouping values.

!   _________________________
!  |                         |
!  | This may take awhile... |
!  |_________________________|
!

IF (l_DoHot) THEN
   IF (i_ActiveCases < i_NumMRVars) THEN
!   _________________________________________________________________________
!  |                                                                         |
!  | 'For Hotelling Commensuration the number of cases must be greater than' |
!  | 'the number of variables'                                               |
!  |_________________________________________________________________________|
!
      call errhand(ch_myMsg,ch_Msg=ch_EM00157)
      l_DoHot = .FALSE.
      iEr = i_NOT_OK
      GOTO 7000
   END IF
   IF (i_NumMRVars == 1) then
!   ___________________________________________________________________
!  |                                                                   |
!  | 'You can only do Hotelling Commensuration with multivariate data' |
!  |___________________________________________________________________|
!
      call errhand(ch_myMsg,ch_Msg=ch_EM00158)
      l_DoHot = .FALSE.
      iEr = i_NOT_OK
      GOTO 7000
   END IF
END IF
IF (l_DoHot) THEN  ! Set i_LZ to carry Hotelling's commens y/n info.
   i_LZ = 1  ! Do the Hotelling's commensuration.
ELSE
   i_LZ = 0
END IF

! num groups, distance upper bound=0, weight c(I),
! Hotelling's Commens (1=Yes,0=No), arc interval.



IF (iEr /= i_OK) THEN
   iEr = i_NOT_OK
   GOTO 7000
END IF

ALLOCATE(  da_XI(1:1),                             &  ! Not needed for EMRPP.
      STAT=ios)
IF (ios /= i_OK) THEN
   iEr = i_NOT_OK
   GOTO 7000
END IF


IF (L_HasExcess) THEN

i_NumXSRecs=ia_GrpSizes(i_NumGroups+1)

END IF


! Check the data
i_NumGps = i_NumGroups
i_NumMVs = i_NumMRVars


IF (l_IsCommens) THEN
   IF (.NOT. l_DoHot) THEN
      CALL commenspp(i_ActiveCases,  & !* <File: "cmsramod.f90: commenspp">
                     i_NumMRVars,    &
                     d_V_DistExp,    &
                     da_Data,       &
                     da_CommAvgDist, &
                     iEr)
      IF (iEr /= i_OK) THEN
         iEr = i_NOT_OK
         GOTO 7000
      END IF
   END IF
END IF
        ! change to I-Beam cursor in Windows                                   !#/WINTERACTER/

! Do the Exact MRPP procedure.
CALL emrpp(i_ActiveCases, i_NumMRVars, d_V_DistExp, i_NumGroups, d_Trunc,     &
      i_CValue, i_LZ, d_ArcIntv, ia_GrpSizes, da_Data, d_Delta1, da_YHot,      &
      d_PValue, iEr) !* <File: "mrppmod.f90: emrpp">

IF (iEr /= i_OK) THEN        ! BEGIN Error handling
   SELECT CASE (iEr)  ! Error handling
      CASE (i_MRPPMOD_ER004)
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |_________________
!  |                                                    |
!  | 'Memory allocation error: da_S,da_C,da_D in EMRPP' |
!  |____________________________________________________|
!
         call errhand(ch_myMsg,i_Code=i_CCode, &
                      ch_Msg=ch_MRPPMOD_EM004)
      CASE (i_MRPPMOD_ER005)
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |____________________
!  |                                                       |
!  | 'Memory allocation error: daYHotHold,daUHot in EMRPP' |
!  |_______________________________________________________|
!
         call errhand(ch_myMsg,i_Code=i_CCode, &
                      ch_Msg=ch_MRPPMOD_EM005)
      CASE (i_MAT_SINGULARITY)
!   _____________________________________________________
!  |                                                     |
!  | 'Either all the elements of some row were zero or'  |
!  | 'a pivot became relatively zero.'                   |
!  | 'The variance/covariance matrix of variable values' |
!  | 'is probably singular and cannot be inverted.'      |
!  | 'Hotelling commensuration cannot be done.'          |
!  |_____________________________________________________|
!
         call errhand(ch_myMsg,i_Code=i_CCode, &
                      ch_Msg=ch_MAT_SINGULARITY)
      CASE (i_MRPPMOD_ER006)
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |________________
!  |                                                   |
!  | 'Memory allocation error: ia_A,ia_P in EMRPPPERM' |
!  |___________________________________________________|
!
         call errhand(ch_myMsg,i_Code=i_CCode, &
                      ch_Msg=ch_MRPPMOD_EM006)
      CASE (i_MRPPMOD_ER007)
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |____________
!  |                                               |
!  | 'Memory allocation error: da_XI in EMRPPDVAL' |
!  |_______________________________________________|
!
         call errhand(ch_myMsg,i_Code=i_CCode, &
                      ch_Msg=ch_MRPPMOD_EM007)
      CASE DEFAULT
   END SELECT
   iEr = i_NOT_OK
   GOTO 7000
END IF                       ! END Error handling

d_Dum = d_ZERO   ! Values not needed for EMRPP.
da_XI = d_ZERO   ! Values not needed for EMRPP.
! Resampling is not done with EXACT MRPP:
i_Seed = 0      ! Values not needed for EMRPP; In fact, TURN THIS OFF.
i_NumPerms = 0  ! Values not needed for EMRPP; In fact, TURN THIS OFF.
l_DoResample = .FALSE. ! Values not needed for EMRPP; In fact, TURN THIS OFF.

7000 CONTINUE

IF (ALLOCATED(da_Values     )) DEALLOCATE(da_Values,      STAT=ios)
IF (ALLOCATED(ia_GrpValTag  )) DEALLOCATE(ia_GrpValTag,   STAT=ios)
IF (ALLOCATED(da_XI         )) DEALLOCATE(da_XI         , STAT=ios)

 IF (iEr /= i_OK) THEN
   call rexit(ch_MyMsg)
END IF

RETURN   ! ... normal exit ...
END SUBROUTINE runemrpp

!==============================================================================
!==============================================================================

SUBROUTINE runmrbp(da_Data, d_V_DistExp, i_NumMRVars, i_NumBlocks,          &
      i_NumGroups, i_NumPerms, da_GpVals, da_BlockV, da_AlignVals,              &
      ch_BlkVarNam, l_IsAligned,l_DoEMRBP,l_DoResample, l_IsCommens, d_ObsDelta, d_ExpDelta,d_VarDelta,d_SkwDelta, &
      d_AgreeVal,d_StdStat, d_PValue,iEr,da_CommAvgDist,l_SaveTest,da_STV,i_Seed) !   &

!___Imported Parameters and Variables:

USE blcmnmod, ONLY: cha_CmdStack, ch_NL, ch_Output_Title, d_ZERO,             &
      ia_ActiveVarNdx, i_NOT_OK, i_OK, i_OUT_OF_RANGE, i_ZERO, l_CLEAR,       &
      l_Terse

        ! ^-- Some global Blossom variables and parameters.

        ! ^-- Misc. Blossom file names, units, status parameters.
USE ioformod, ONLY: ch12_OutBuff, ch12_OutBuff2, ch12_OutBuff3
        ! ^-- Output buffer for charcter output of INTEGER number.
USE jonsmodule, ONLY: ch_EM00033, ch_EM00047, ch_EM00164, ch_EM00165,         &
      ch_EM00192, ch_MRPPMOD_EM010, ch_MRPPMOD_EM011, ch_MRPPMOD_EM012,       &
      ch_MRPPMOD_EM013, ch_MRPPMOD_EM014, ch_MRPPMOD_EM015, ch_MRPPMOD_EM016, &
      ch_MRPPMOD_EM017, ch_MRPPMOD_EM018, ch_MRPPMOD_EM019, ch_MRPPMOD_EM020, &
      ch_MRPPMOD_EM021, ch_MRPPMOD_EM022, ch_MRPPMOD_EM023, ch_MRPPMOD_EM025, &
      ch_STR0013, i_CCode, i_MRPPMOD_ER010, i_MRPPMOD_ER011, i_MRPPMOD_ER012, &
      i_MRPPMOD_ER013, i_MRPPMOD_ER014, i_MRPPMOD_ER015, i_MRPPMOD_ER016,     &
      i_MRPPMOD_ER017, i_MRPPMOD_ER018, i_MRPPMOD_ER019, i_MRPPMOD_ER020,     &
      i_MRPPMOD_ER021, i_MRPPMOD_ER022, i_MRPPMOD_ER023, i_MRPPMOD_ER025
        ! ^-- Message handling, etc.
USE mrolamod, ONLY: i_MAX_EMRBP_BLOCKS, i_MIN_EMRBP_BLOCKS,                   &
      i_MIN_NUM_BKS_MRBP, i_MIN_NUM_GPS_MRBP,i_USE_SYS_RAND
        ! ^-- Permutation, regression and other Blossom stat parameters.
USE mrauxmod, ONLY: alignblkdata
USE jonsmodule, ONLY:  errhand, getrseed
        ! ^-- emitstr: Display a string to the console output stream.

USE mt19937, ONLY: init_genrand
        ! ^-- Mersenne Twister Pseudo Random Number Generator
IMPLICIT NONE
!___Dummy Arguments:

REAL (KIND(0D0)),  INTENT(IN)    :: d_V_DistExp
REAL (KIND(0D0)),  INTENT(INOUT)    :: da_Data(:,:,:)
INTEGER,           INTENT(IN)    :: i_NumMRVars
INTEGER,           INTENT(IN)    :: i_NumBlocks
INTEGER,           INTENT(IN)    :: i_NumGroups
INTEGER,           INTENT(IN)    :: i_NumPerms
LOGICAL, 		   INTENT(IN)    :: l_IsAligned
LOGICAL, 		   INTENT(INOUT)    :: l_IsCommens
LOGICAL, 		   INTENT(INOUT)     :: l_DoEMRBP
LOGICAL, 		   INTENT(INOUT)    :: l_DoResample

REAL (KIND(0D0)),  INTENT(IN)    :: da_GpVals(:) ! dim(i_NumCases)
REAL (KIND(0D0)),  INTENT(IN)    :: da_BlockV(:) ! dim(i_NumCases)
REAL (KIND(0D0)),  INTENT(OUT)   :: da_AlignVals(:,:)
                  ! dim(i_NumBlocks,i_NumMRVars)--^
CHARACTER (LEN=*), INTENT(IN)    :: ch_BlkVarNam
INTEGER,           INTENT(OUT)   :: iEr
REAL (KIND(0D0)),  INTENT(OUT)    :: d_ObsDelta
REAL (KIND(0D0)),  INTENT(OUT)    :: d_ExpDelta
REAL (KIND(0D0)),  INTENT(OUT)    :: d_VarDelta
REAL (KIND(0D0)),  INTENT(OUT)    :: d_SkwDelta
REAL (KIND(0D0)),  INTENT(OUT)    :: d_AgreeVal
REAL (KIND(0D0)),  INTENT(OUT)    :: d_StdStat
REAL (KIND(0D0)),  INTENT(OUT)    :: d_PValue
REAL (KIND(0D0)), INTENT(OUT) :: da_CommAvgDist(:)
REAL (KIND(0D0)), INTENT(OUT) :: da_STV(:)  ! save test values
INTEGER,          INTENT(INOUT)    :: i_seed
LOGICAL,  		   INTENT (IN)   :: l_SaveTest
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     SUBROUTINE runmrbp
! DESCRIPTION:
!     Setup, run MRBP and Exact MRBP routines then display results from
!     this routine. JDR  31 Mar 2000
!     NOTE: Variables i_NumGVVals,l_HasGVals, and
!           isaGpSize are not "used" in this  subroutine, but are "passed"
!           on to called subroutines herein. -JDR
! LANGUAGE:
!     Fortran 90/95
! AUTHOR:
!     Jon D. Richards, Operations Research Analyst       jon_richards@usgs.gov
!     Fort Collins Science Center                     http://www.fort.usgs.gov
!     U.S. Geological Survey          -          Biological Resources Division
! INVOKED BY:
!     runmrprocs      <File: "domrpp.f90: runmrprocs">        module
! INVOKES:
!     alignblkdata    <File: "mrauxmod.f90: alignblkdata">    module
!     chkmrpp         <File: "domrpp.f90: chkmrpp">           module
!     commensbp       <File: "cmsramod.f90: commensbp">       module
!     dispmrbp        <File: "domrpp.f90: dispmrbp">          module
!     emitstr         <File: "jonsmodule.f90: emitstr">       module
!     emrbp           <File: "mrppmod.f90: emrbp">            module
!     errhand         <File: "jonsmodule.f90: errhand">       module
!     getactivevarlst <File: "mrauxmod.f90: getactivevarlst"> module
!     getrseed        <File: "jonsmodule.f90: getrseed">      module
!     mrbp            <File: "mrppmod.f90: mrbp">             module

! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!___Procedure Parameters:
CHARACTER (LEN=*), PARAMETER :: ch_PROC_NAME = "RUNMRBP"
!___Local Variables:

REAL (KIND(0D0)) :: d_H = d_ZERO

INTEGER :: i, k
INTEGER :: ios
INTEGER :: i_Exc
INTEGER :: i_IA   ! If 0 then don't do alignment in emrbp procedure.
INTEGER :: i_IC   ! If 0 then don't do commensuration in emrbp procedure.
INTEGER :: i_LR   ! If 0 then don't do Ranks Test in emrbp procedure.
INTEGER :: i_TotNumPerms
CHARACTER (LEN=4) :: blcmd
INTEGER, PARAMETER :: i_LEN_MY_EMSG = 254 ! needs to be this big for output of var list in dostatus.
CHARACTER (LEN=i_LEN_MY_EMSG) :: ch_MyMsg
!___Intrinsic Procedures:
INTRINSIC :: ALLOCATED, RANDOM_SEED, SIZE
!___Executable Statements:

iEr = i_OK ! no problem yet
d_H = 0.0D0 ! d_ZERO

! Get active variable index array (array containing the index numbers
! of var list that are in mrvar list).


CALL alignblkdata(da_BlockV,    & !* <File: "mrauxmod.f90: alignblkdata">
                  i_NumBlocks,  &
                  i_NumMRVars,  &
                  i_NumGroups,  &
                  l_IsAligned,  &
                  da_Data,      &
                  da_AlignVals, &
                  iEr,          &
                  ch_MyMsg)

! Check the data for out limits and rules.
CALL chkmrpp(ch_MyMsg,iEr) !* <File: "domrpp.f90: chkmrpp">
IF (iEr /= i_OK) THEN
   iEr = i_NOT_OK
   GOTO 70000
END IF

IF (l_IsCommens) THEN  ! If commensuration is to be done, do so.

   CALL commensbp(i_NumGroups,    & !* <File: "cmsramod.f90: commensbp">
                  i_NumBlocks,    &
                  i_NumMRVars,    &
                  d_V_DistExp,    &
                  da_Data,        &
                  da_CommAvgDist, &
                  iEr)
   IF (iEr /= i_OK) THEN
      iEr = i_NOT_OK
      GOTO 70000
   END IF
ELSE
   da_CommAvgDist = d_ZERO ! Data is not commensurated. (not applicable)
END IF

IF (.NOT.l_DoEMRBP) THEN
CALL mrbp(d_V_DistExp, i_NumGroups, i_NumBlocks, i_NumMRVars, i_NumPerms,  &
         l_DoResample, da_Data, d_ObsDelta, d_ExpDelta, d_VarDelta,           &
         d_SkwDelta, d_AgreeVal, d_StdStat, d_PValue, iEr,l_SaveTest,da_STV) !* <File: "mrppmod.f90: mrbp">

i_TotNumPerms = 0 ! (not applicable)
ELSE ! If EXACT MRBP, do so.                                                   !#/WINTERACTER/

   ! Do the Exact MRBP procedure.
   i_IA = i_ZERO  ! Don't do alignment in emrbp procedure.
   i_IC = i_ZERO  ! Don't do commensuration in emrbp procedure.
   i_LR = i_ZERO  ! Don't do Ranks Test in emrbp procedure.
   CALL emrbp(d_V_DistExp, i_NumGroups, i_NumBlocks, i_NumMRVars, i_IA, i_IC, &
         i_LR, d_H, da_Data, i_TotNumPerms, d_ObsDelta, d_PValue, iEr)
        !* <File: "mrppmod.f90: emrbp">
   ! Set these variables for Exact MRBP (not operational for EMRBP):
!   i_NumPerms   = i_ZERO  ! (not applicable)
   i_Seed       = i_ZERO  ! (not applicable)
   l_DoResample = .FALSE. ! (not applicable)
   d_ExpDelta   = d_ZERO  ! (not applicable)
   d_VarDelta   = d_ZERO  ! (not applicable)
   d_SkwDelta   = d_ZERO  ! (not applicable)
   d_AgreeVal   = d_ZERO  ! (not applicable)
   d_StdStat    = d_ZERO  ! (not applicable)
END IF
IF (iEr /= i_OK) THEN   ! BEGIN Error handling
   SELECT CASE (iEr)  ! Error handling
      CASE (i_MRPPMOD_ER010)
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |____________
!  |                                               |
!  | 'Memory allocation error: da_Dis in MRBPCALC' |
!  |_______________________________________________|
!
         CALL errhand(ch_myMsg,i_Code=i_CCode, &
                      ch_Msg=ch_MRPPMOD_EM010)
      CASE (i_MRPPMOD_ER011)
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |_________________________
!  |                                                            |
!  | 'Memory allocation error: da_SJ,da_SJ2,da_SJ3,da_SIJ,      |
!  |  da_SIJ2,da_SIJ3,da_UIJ,da_TIJ2,da_TIJ3,da_VI in MRBPCALC' |
!  |____________________________________________________________|
!
       CALL errhand(ch_myMsg,i_Code=i_CCode, &
                      ch_Msg=ch_MRPPMOD_EM011)
      CASE (i_MRPPMOD_ER012)
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |______________________
!  |                                                         |
!  | 'Memory allocation error: da_AD, da_XM,da_X,da_S,ia_P2, |
!  |  ia_P3,ia_P4,ia_P5,ia_P6,ia_P7,ia_P8,ia_P9 IN EMRBP'    |
!  |_________________________________________________________|
!
       CALL errhand(ch_myMsg,i_Code=i_CCode, &
                      ch_Msg=ch_MRPPMOD_EM012)
      CASE (i_MRPPMOD_ER014)
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |_____________
!  |                                                |
!  | 'Memory allocation error: da_RKS in EMRBPRANK' |
!  |________________________________________________|
!
         CALL errhand(ch_myMsg,i_Code=i_CCode, &
                      ch_Msg=ch_MRPPMOD_EM014)
      CASE (i_MRPPMOD_ER015)
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |_______
!  |                                          |
!  | 'Memory allocation error: ia_A in FACT2' |
!  |__________________________________________|
!
         CALL errhand(ch_myMsg,i_Code=i_CCode, &
                      ch_Msg=ch_MRPPMOD_EM015)
      CASE (i_MRPPMOD_ER016)
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |_______
!  |                                          |
!  | 'Memory allocation error: ia_A in FACT3' |
!  |__________________________________________|
!
        CALL errhand(ch_myMsg,i_Code=i_CCode, &
                      ch_Msg=ch_MRPPMOD_EM016)
      CASE (i_MRPPMOD_ER017)
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |_______
!  |                                          |
!  | 'Memory allocation error: ia_A in FACT4' |
!  |__________________________________________|
!
        CALL errhand(ch_myMsg,i_Code=i_CCode, &
                      ch_Msg=ch_MRPPMOD_EM017)
      CASE (i_MRPPMOD_ER018)
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |_______
!  |                                          |
!  | 'Memory allocation error: ia_A in FACT5' |
!  |__________________________________________|
!
        CALL errhand(ch_myMsg,i_Code=i_CCode, &
                      ch_Msg=ch_MRPPMOD_EM018)
      CASE (i_MRPPMOD_ER019)
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |_______
!  |                                          |
!  | 'Memory allocation error: ia_A in FACT6' |
!  |__________________________________________|
!
       CALL errhand(ch_myMsg,i_Code=i_CCode, &
                      ch_Msg=ch_MRPPMOD_EM019)
      CASE (i_MRPPMOD_ER020)
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |_______
!  |                                          |
!  | 'Memory allocation error: ia_A in FACT7' |
!  |__________________________________________|
!
         CALL errhand(ch_myMsg,i_Code=i_CCode, &
                      ch_Msg=ch_MRPPMOD_EM020)
      CASE (i_MRPPMOD_ER021)
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |_______
!  |                                          |
!  | 'Memory allocation error: ia_A in FACT8' |
!  |__________________________________________|
!
         CALL errhand(ch_myMsg,i_Code=i_CCode, &
                      ch_Msg=ch_MRPPMOD_EM021)
      CASE (i_MRPPMOD_ER022)
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |_______
!  |                                          |
!  | 'Memory allocation error: ia_A in FACT9' |
!  |__________________________________________|
!
         CALL errhand(ch_myMsg,i_Code=i_CCode, &
                      ch_Msg=ch_MRPPMOD_EM022)
      CASE (i_MRPPMOD_ER023)
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |___________
!  |                                              |
!  | 'Memory allocation error: ia_A in EMRBPDVAL' |
!  |______________________________________________|
!
         CALL errhand(ch_myMsg,i_Code=i_CCode, &
                     ch_Msg=ch_MRPPMOD_EM023)
      CASE (i_MRPPMOD_ER025)
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |_______
!  |                                          |
!  | 'Memory allocation error: da_DT in MRBP' |
!  |__________________________________________|
!
         CALL errhand(ch_myMsg,i_Code=i_CCode, &
                      ch_Msg=ch_MRPPMOD_EM025)
      CASE DEFAULT
   END SELECT
   iEr = i_NOT_OK
   GOTO 70000
END IF                  ! END Error handling
! ########  NOTE: there should be no missing data allowed with MRBP! ##########
! #   If there was a HOLD file, delete the current TEMP file and rename the   #
! #   HOLD file to be the TEMP file.                                          #
! #############################################################################



70000 CONTINUE

80000 CONTINUE

 IF (iEr /= i_OK) THEN
   call rexit(ch_MyMsg)
END IF
!l_SAVETEST = .FALSE. ! done with this until next time it is invoked
RETURN   ! ... normal and error exits here ...
END SUBROUTINE runmrbp



SUBROUTINE runptmp(i_NC, d_V_DistExp, iPRank, dPRankExp, da_D1, da_D2, &
            d_ExpDelta, d_VarDelta, d_SkwDelta, d_StdStat, d_Rho, d_ObsDelta, &
            d_PValue, l_DoResample,l_ExactPTMPDone, i_NumPerms,iEr,l_SaveTest,da_STV,i_seed)

!d USE debugmod, ONLY: MyDebug                                         ! debug!
!___Imported Parameters and Variables:


USE blcmnmod, ONLY: cha_CmdStack, ch_BLANK, ch_NL, ch_Output_Title, d_ZERO,   &
      ia_ActiveVarNdx, i_NOT_OK, i_NumVars, i_OK, l_CLEAR, l_Terse
        ! ^-- Some global Blossom variables and parameters.
USE ioformod, ONLY: ch12_OutBuff, ch12_OutBuff2
        ! ^-- Output buffer for character output of INTEGER number.
USE jonsmodule, ONLY: ch_EM00033, ch_EM00047, ch_EM00166, ch_EM00192,         &
      ch_MRPPMOD_EM008, ch_MRPPMOD_EM009, ch_MRPPMOD_EM024, i_CCode,          &
      i_MRPPMOD_ER008, i_MRPPMOD_ER009, i_MRPPMOD_ER024
        ! ^-- Message handling, etc.
USE mrolamod, ONLY: i_MIN_NUM_PTMP_CASES, i_USE_SYS_RAND
        ! ^-- Permutation, regression and other Blossom stat parameters.
!___Imported Procedures:

USE jonsmodule, ONLY: errhand, getrseed, lf_Equals
        ! ^-- delfil: Delete a file.
        ! ^-- getrseed: get random number seed.
        ! ^-- renfil: Rename a file.
        ! ^-- lf_Equals: Generic test for equality of two objects.


IMPLICIT NONE
!___Dummy Arguments:

INTEGER,          INTENT(IN)  :: i_NC
REAL (KIND(0D0)), INTENT(IN)  :: d_V_DistExp
INTEGER,          INTENT(IN)  :: iPRank  ! power of ranks? 1=Y,2=N
REAL (KIND(0D0)), INTENT(IN)  :: dPRankExp
REAL (KIND(0D0)), INTENT(IN)  :: da_D1(:)
REAL (KIND(0D0)), INTENT(IN)  :: da_D2(:)
!These options control which ptmp is performed
INTEGER,          INTENT(IN)  :: i_NumPerms
LOGICAL,          INTENT(IN)  :: l_DoResample
LOGICAL,          INTENT(IN)  :: l_ExactPTMPDone
!output changes for different exact vs. resample
REAL (KIND(0D0)), INTENT(OUT) :: d_ExpDelta
REAL (KIND(0D0)), INTENT(OUT) :: d_VarDelta
REAL (KIND(0D0)), INTENT(OUT) :: d_SkwDelta
REAL (KIND(0D0)), INTENT(OUT) :: d_StdStat
REAL (KIND(0D0)), INTENT(OUT) :: d_Rho
REAL (KIND(0D0)), INTENT(OUT) :: d_ObsDelta
REAL (KIND(0D0)), INTENT(OUT) :: d_PValue
INTEGER,          INTENT(OUT) :: iEr
REAL (KIND(0D0)), INTENT(OUT) :: da_STV(:)  ! save test values
INTEGER,          INTENT(INOUT)    :: i_seed
LOGICAL,  		   INTENT (IN)   :: l_SaveTest
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     SUBROUTINE runptmp
! DESCRIPTION:
!     Run a Permutation Test for Matched Pairs.
!     For PTMP, there are no grouping or blocking variables, no
!     sorting was done, and no index arrays were created.
!     We have a count of the number of cases, which here will become
!     the number of pairs.
!     We may have a distance exponent other than the default.
!     PTMP allows for power of ranks test, but we will not be using this.
! LANGUAGE:
!     Fortran 90/95
! AUTHOR:
!     Jon D. Richards, Operations Research Analyst       jon_richards@usgs.gov
!     Fort Collins Science Center                     http://www.fort.usgs.gov
!     U.S. Geological Survey          -          Biological Resources Division
! INVOKED BY:
!     runmrprocs      <File: "domrpp.f90: runmrprocs">        module
! INVOKES:
!     chkmrpp         <File: "domrpp.f90: chkmrpp">           module
!     delfil          <File: "jonsmodule.f90: delfil">        module
!     dispptmp        <File: "domrpp.f90: dispptmp">          module
!     errhand         <File: "jonsmodule.f90: errhand">       module
!     getactivevarlst <File: "mrauxmod.f90: getactivevarlst"> module
!     getrseed        <File: "jonsmodule.f90: getrseed">      module
!     ptmp            <File: "mrppmod.f90: ptmp">             module
!     renfil          <File: "jonsmodule.f90: renfil">        module
!     sptmp           <File: "mrppmod.f90: sptmp">            module

! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!___Procedure Parameters:
CHARACTER (LEN=*), PARAMETER ::  ch_PROB_MSG = "Problem reading TEMP file"
CHARACTER (LEN=*), PARAMETER :: ch_PROC_NAME = "RUNPTMP"

!___Local Variables:

INTEGER :: i, j, k
INTEGER :: ios
INTEGER :: i_Exc
INTEGER :: i_NumNonZero
INTEGER :: i_ng
INTEGER :: i_NumRcd
LOGICAL :: l_IsCommens
CHARACTER (LEN=4) :: blcmd
INTEGER, PARAMETER :: i_LEN_MY_EMSG = 254 ! needs to be this big for output of var list in dostatus.
CHARACTER (LEN=i_LEN_MY_EMSG) :: ch_MyMsg
!___Intrinsic Procedures:
INTRINSIC :: ALLOCATED, RANDOM_SEED, SIZE
!___Executable Statements:

iEr = i_OK ! no problem yet

l_IsCommens = .FALSE.  ! For now, no commensuration for PTMP.

IF (l_DoResample) THEN
   ! Do the resample PTMP procedure.
   CALL sptmp(i_NumPerms,   & !* <File: "mrppmod.f90: sptmp">
              i_NC,         &
              d_V_DistExp,  &
              da_D1,        &
              da_D2,        &
              d_ObsDelta,   &
              d_PValue,     &
!               i_NumNonZero, &
              iEr,l_SaveTest,da_STV,i_seed)
   d_Rho      = d_ZERO
   d_SkwDelta = d_ZERO
   d_StdStat  = d_ZERO
   d_VarDelta = d_ZERO

ELSE

   ! Here I am keeping in the 2d distance matrix version in case we want
   ! to invoke it later. For now, we will only use the 1-D version
   ! Do the PTMP procedure.

      CALL ptmp(i_NC, d_V_DistExp, iPRank, dPRankExp, da_D1, da_D2, &
            d_ExpDelta, d_VarDelta, d_SkwDelta, d_StdStat, d_Rho, d_ObsDelta, &
            d_PValue, l_ExactPTMPDone, iEr) !* <File: "mrppmod.f90: ptmp">

END IF
IF (iEr /= i_OK) THEN   ! BEGIN Error Handling
   SELECT CASE (iEr)  ! Error handling
      CASE (i_MRPPMOD_ER008)
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |__________________________
!  |                                                             |
!  | 'Memory allocation error: da_X,da_Y,da_R,da_A,da_B in PTMP' |
!  |_____________________________________________________________|
!
        CALL errhand(ch_myMsg,i_Code=i_CCode, &
                      ch_Msg=ch_MRPPMOD_EM008)
      CASE (i_MRPPMOD_ER024)
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |_______
!  |                                          |
!  | 'Memory allocation error: da_X in SPTMP' |
!  |__________________________________________|
!
         CALL errhand(ch_myMsg,i_Code=i_CCode, &
                      ch_Msg=ch_MRPPMOD_EM024)
      CASE DEFAULT
   END SELECT
   iEr = i_NOT_OK
   GOTO 7000
END IF                  ! END Error Handling

7000 CONTINUE
IF (iEr /= i_OK) THEN
   iEr = i_NOT_OK
END IF

 IF (iEr /= i_OK) THEN
   call rexit(ch_MyMsg)
END IF

RETURN   ! ... normal exit ...
END SUBROUTINE runptmp

!=============================================================================!

SUBROUTINE chkmrpp(ch_MyMsg,iEr)

!___Imported Parameters and Variables:
USE blcmnmod, ONLY: ch_NL, i_NOT_OK, i_OK
        ! ^-- Some global Blossom variables and parameters.
USE ioformod, ONLY: ch12_OutBuff, ch12_OutBuff2, ch12_OutBuff3,               &
      ch12_OutBuff4, ch12_OutBuff5, ch12_OutBuff6, ch12_OutBuff7,             &
      ch12_OutBuff8
        ! ^-- Output buffer for character output of INTEGER number.
USE jonsmodule, ONLY: ch_EM00167, ch_EM00168, ch_EM00169, ch_EM00170,         &
      ch_EM00171, ch_EM00172, ch_EM00173, ch_EM00174, ch_EM00175, ch_STR0013
        ! ^-- Message handling, etc.      
USE mrolamod, ONLY: ia_MAX_DIM_AN_PROG, i_AGREE_LIMIT, i_BLK_LIMIT,           &
      i_EMRPP_LIMIT, i_GRP_LIMIT, i_MAX_EMRBP_BLOCKS, i_MRBP_LIMIT,           &
      i_MRPP_LIMIT, i_NUM2DO_EPTMP, i_NumBks, i_NumGps, i_NumMVs, i_NumRcd,   &
      i_OBS_LIMIT, i_PAIRED_LIMIT, i_ProgType, i_PROG_TYPE_EMRPP,             &
      i_PROG_TYPE_MRBP, i_PROG_TYPE_MRPP, i_PROG_TYPE_PTMP, i_PTMP_LIMIT,     &
      i_VAR_LIMIT, l_DoEMRBP, l_IsAgBP, l_IsBPBP, l_IsPaBP
        ! ^-- Permutation, regression and other Blossom stat parameters.

USE jonsmodule, ONLY: errhand
        ! ^-- emitstr: Display Blossom command prompt.
        ! ^-- errhand: Display error message.
IMPLICIT NONE
!___Dummy Arguments:
CHARACTER (LEN=*), OPTIONAL, INTENT(OUT) :: ch_MyMsg
INTEGER, INTENT(OUT) :: iEr

! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     SUBROUTINE chkmrpp
! DESCRIPTION:
!     Run the appropriate program for mrpp (MRPP, EMRPP, PTMP, MRBP).
!     Check to see that dimensions of application program are not exceeded.
! LANGUAGE:
!     Fortran 90/95
! AUTHOR:
!     Jon D. Richards, Operations Research Analyst       jon_richards@usgs.gov
!     Fort Collins Science Center                     http://www.fort.usgs.gov
!     U.S. Geological Survey          -          Biological Resources Division
! INVOKED BY:
!     runemrpp      <File: "domrpp.f90: runemrpp">        module
!     runmrbp       <File: "domrpp.f90: runmrbp">         module
!     runmrpp       <File: "domrpp.f90: runmrpp">         module
!     runptmp       <File: "domrpp.f90: runptmp">         module
! INVOKES:
!     emitstr       <File: "jonsmodule.f90: emitstr">     module
!     errhand       <File: "jonsmodule.f90: errhand">     module

iEr = i_OK ! no problem yet
l_IsAgBP = .FALSE.
l_IsBPBP = .FALSE.
l_IsPaBP = .FALSE.

      IF (ia_MAX_DIM_AN_PROG(i_MRBP_LIMIT,i_OBS_LIMIT) >= i_NumRcd  .AND.     &
          ia_MAX_DIM_AN_PROG(i_MRBP_LIMIT,i_GRP_LIMIT) >= i_NumGps  .AND.     &
          ia_MAX_DIM_AN_PROG(i_MRBP_LIMIT,i_VAR_LIMIT) >= i_NumMVs  .AND.     &
          ia_MAX_DIM_AN_PROG(i_MRBP_LIMIT,i_BLK_LIMIT) >= i_NumBks) THEN
         l_IsBPBP = .TRUE.
      ELSE IF (ia_MAX_DIM_AN_PROG(i_AGREE_LIMIT,i_OBS_LIMIT) >= i_NumRcd      &
        .AND.  ia_MAX_DIM_AN_PROG(i_AGREE_LIMIT,i_GRP_LIMIT) >= i_NumGps      &
        .AND.  ia_MAX_DIM_AN_PROG(i_AGREE_LIMIT,i_VAR_LIMIT) >= i_NumMVs      &
        .AND.  ia_MAX_DIM_AN_PROG(i_AGREE_LIMIT,i_BLK_LIMIT) >= i_NumBks) THEN
         l_IsAgBP = .TRUE.    ! agree mrbp
      ELSE IF (ia_MAX_DIM_AN_PROG(i_PAIRED_LIMIT,i_OBS_LIMIT) >= i_NumRcd     &
        .AND.  ia_MAX_DIM_AN_PROG(i_PAIRED_LIMIT,i_GRP_LIMIT) >= i_NumGps     &
        .AND.  ia_MAX_DIM_AN_PROG(i_PAIRED_LIMIT,i_VAR_LIMIT) >= i_NumMVs     &
        .AND.  ia_MAX_DIM_AN_PROG(i_PAIRED_LIMIT,i_BLK_LIMIT) >= i_NumBks)    &
            THEN
         l_IsPaBP = .TRUE.    ! paired, ptmp
      END IF      
RETURN   ! ... normal and error exits here ...
END SUBROUTINE chkmrpp
END MODULE mrppmod




MODULE medqmod
!d USE mrolamod, ONLY: l_DoMedQ                                        ! debug!
        ! ^-- Permutation, regression and other Blossom stat parameters.
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     MODULE medqmod
! DESCRIPTION:
!     Subroutines for the multi-response medians and quantiles computation
!        MEDQUANT (driver subroutine), MEDQ, RMEDQ
! LANGUAGE:
!     Fortran 90/95
! AUTHOR:
!     Jon D. Richards, Operations Research Analyst       jon_richards@usgs.gov
!     Fort Collins Science Center                     http://www.fort.usgs.gov
!     U.S. Geological Survey          -          Biological Resources Division
!     Original programs from Dr. Paul Mielke, Colo. State Univ.
!        http://www.stat.colostate.edu/~mielke/mielkesoftware.html

! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
IMPLICIT NONE
SAVE
!___Module Parameters:
CHARACTER (LEN=*), PARAMETER :: ch_PROB_MSG          = "Problem in MEDQUANT, "
        ! Number of variables in univariate case:
INTEGER,           PARAMETER :: i_UNIVARIATE         = 1
        ! Iteration limit for mulitvariate computations
INTEGER,           PARAMETER :: i_ITERATION_LIMIT    = 500

        ! Default number of Quantiles to do:
INTEGER,           PARAMETER :: i_DEFAULT_NUM_QUANTS = 9
        ! Tolerance (decimal points) for MEDQ:
REAL (KIND(0D0)),  PARAMETER :: d_DEF_MEDQ_TOLERANCE = 0.100000000000000D-013

        ! Default quantile values to use:
REAL (KIND(0D0)),  PARAMETER :: da_DEFAULT_QUANT_VALS(i_DEFAULT_NUM_QUANTS) &
   = (/0.0D0, 0.05D0, 0.10D0, 0.25D0, 0.50D0, 0.75D0, 0.90D0, 0.95D0, 1.0D0 /)
!___Module Variables:

LOGICAL :: l_HasSpQList
        ! Whether data is grouped (medq)
LOGICAL :: l_IsGrouped
        ! The rmedquant (r-way medians/quantiles) is to use the
        ! Default quantile list.
LOGICAL :: l_UseDefQList
        ! Whether a file is to be saved of the medians from
        ! an rmedquat analysis.
LOGICAL :: l_DistSave
        ! If Distance file exists, append to it.
LOGICAL :: l_Append2DistFile
        ! Quantile values for which we want answers for from rmedquant,
        ! r-way medians and quantiles.

CONTAINS
!     MEDQUANT <File: "medqmod.f90: medquant">
!     MEDQ     <File: "medqmod.f90: medq">
!     RMEDQ    <File: "medqmod.f90: rmedq">

!=============================================================================!
SUBROUTINE runmedq(ia_GpSize,       &
                   i_NumQuantVals,  &
                   da_QuantVals,    &
                   i_NumGroups,     &
                   i_NumCases,      &
                   i_NumVars,       &
                   da_Data,                  &
                   da_VariableWInGpMedian,   &
                   da_GpAvgDistToGpMVMedian, &
                   da_GpMedQTolerance,       &
                   da_WInGrpVarEstimateVal,  &
                   da_WInGpQuantDist,        &
                   da_ObsDistToGpMedian,     &
                   ia_NumIterations,         &
                   da_GpVals,       &
                   iEr)

USE blcmnmod, ONLY: ch_BLANK, ch_NL, d_ZERO, ia_ActiveVarNdx, i_NOT_OK,i_OK, l_CLEAR, l_EchoOutput
        ! ^-- Some global Blossom variables and parameters.
USE jonsmodule, ONLY: ch_EM00033, ch_EM00047, ch_MEDQMOD_EM001,               &
      ch_MEDQMOD_EM002, ch_MEDQMOD_EM003, i_CCode, i_MEDQMOD_ER001,           &
      i_MEDQMOD_ER002, i_MEDQMOD_ER003, ch_STR0030
        ! ^-- Message handling, etc.

USE mrolamod, ONLY: i_NumRcd, l_HasGVals
        ! ^-- Permutation, regression and other Blossom stat parameters.
!___Imported Procedures:

USE jonsmodule, ONLY:  errhand
        ! ^-- delfil: Delete a file.
        ! ^-- renfil: Rename a file.


IMPLICIT NONE
!___Dummy Arguments:
INTEGER,           INTENT(INOUT) :: ia_GpSize(:)       ! dim(i_NumCases)
INTEGER, 		   INTENT(INOUT) :: i_NumQuantVals
REAL (KIND((0D0))), INTENT(INOUT) :: da_QuantVals(:)
INTEGER,           INTENT(INOUT) :: i_NumGroups
INTEGER,           INTENT(IN)    :: i_NumCases
INTEGER,           INTENT(IN)    :: i_NumVars
REAL (KIND(0D0)),  INTENT(OUT)   :: da_GpVals(:)       ! dim(i_NumCases)
INTEGER,           INTENT(OUT)   :: iEr
REAL (KIND(0D0)), INTENT(INOUT) :: da_Data(:,:)
REAL (KIND(0D0)), INTENT(OUT)   :: da_VariableWInGpMedian(:,:)
REAL (KIND(0D0)), INTENT(OUT)   :: da_GpAvgDistToGpMVMedian(:)
REAL (KIND(0D0)), INTENT(OUT)   :: da_GpMedQTolerance(:)
REAL (KIND(0D0)), INTENT(OUT)   :: da_WInGrpVarEstimateVal(:,:)
REAL (KIND(0D0)), INTENT(OUT)   :: da_WInGpQuantDist(:,:)
REAL (KIND(0D0)), INTENT(OUT)   :: da_ObsDistToGpMedian(:,:)
INTEGER,          INTENT(OUT)   :: ia_NumIterations(:)

!x INTEGER, INTENT(OUT) :: i_ActiveCases
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     SUBROUTINE runmedq
! DESCRIPTION:
!     Acquire data, run the median/quantile routines, display results.
! LANGUAGE:
!     Fortran 90/95
! AUTHOR:
!     Jon D. Richards, Operations Research Analyst       jon_richards@usgs.gov
!     Fort Collins Science Center                     http://www.fort.usgs.gov
!     U.S. Geological Survey          -          Biological Resources Division


! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!___Procedure Parameters:
CHARACTER (LEN=*), PARAMETER :: ch_PROC_NAME = "RUNMEDQ"
!___Local Variables:



REAL (KIND(0D0)) :: d_ExcessVal
INTEGER :: i, j, k
INTEGER :: ios
INTEGER :: i_ActiveCases, i_MaxGPSize
INTEGER :: i_Exc
LOGICAL :: l_HasExcess, l_HasXsVal
LOGICAL :: l_ToOutFile, l_ToScreen, l_Advance
INTEGER, PARAMETER :: i_LEN_MY_EMSG = 254 ! needs to be this big for output of var list in dostatus.
CHARACTER (LEN=i_LEN_MY_EMSG) :: ch_MyMsg
!___Intrinsic Procedures:
INTRINSIC :: ALLOCATED, MAXVAL
!___Executable Statements:

iEr = i_OK ! no problem yet



! Check the specifications and data.
CALL chkmedq(i_NumGroups,    & !* <File: "domedq.f90: chkmedq">
             i_NumVars,    &
             i_NumQuantVals, &
             da_QuantVals,  &
             i_NumCases,  &
             ia_GpSize,      &
             iEr,ch_MyMsg)
IF (iEr /= i_OK) THEN
   iEr = i_NOT_OK
   RETURN   ! ... error exit ...
END IF


! Find the multivariate median and quantile values.
CALL medquant(i_NumVars,              &
              i_NumGroups,              &
              ia_GpSize,                &
              i_NumQuantVals,	        &
              da_QuantVals,   &
              da_Data,                  &
              da_VariableWInGpMedian,   &
              da_GpAvgDistToGpMVMedian, &
              da_GpMedQTolerance,       &
              da_WInGrpVarEstimateVal,  &
              da_WInGpQuantDist,        &
              da_ObsDistToGpMedian,     &
              ia_NumIterations,         &
              iEr,ch_MyMsg)
              
! /////// for time being ignore problems from medquant ////////////

IF (iEr /= i_OK) THEN
   SELECT CASE (iEr)  ! Error handling
      CASE (i_MEDQMOD_ER001)
!   ______________________________________________________________________________
!  |                                                                              |
!  | 'Problem in MEDQUANT, You specified a quantile outside the range 0.0 to 1.0' |
!  |______________________________________________________________________________|
!
         CALL errhand(Ch_MyMsg,ch_Msg=ch_MEDQMOD_EM001)
      CASE (i_MEDQMOD_ER002)
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |_______
!  |                                          |
!  | 'Memory allocation error: daV in RMEDQ1' |
!  |__________________________________________|
!
         CALL errhand(Ch_MyMsg, i_Code=i_CCode, &
                      ch_Msg=ch_MEDQMOD_EM002)
      CASE (i_MEDQMOD_ER003)
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |________
!  |                                           |
!  | 'Memory allocation error: da_W1 in RMEDQ' |
!  |___________________________________________|
!
         CALL errhand(Ch_MyMsg, i_Code=i_CCode,        &
                      ch_Msg=ch_MEDQMOD_EM003)
      ! Don't do this here:
      !       CASE (i_MEDQMOD_ER004)
      ! !   __________________________________
      ! !  |                                  |
      ! !  | IOSTAT_MSG(i_Code) error message |__________________
      ! !  |                                                     |
      ! !  | 'Memory allocation error: da_QuantVals in MEDQUANT' |
      ! !  |_____________________________________________________|
      ! !
      !          CALL errhand(Ch_MyMsg, i_Code=i_CCode, ch_Msg=ch_MEDQMOD_EM004)
      CASE default
   END SELECT
   RETURN   ! ... error exit ...
END IF

! Display results of RMEDQ

 IF (iEr /= i_OK) THEN
   call rexit(ch_MyMsg)
END IF

RETURN
END SUBROUTINE runmedq
!=====================================================
SUBROUTINE chkmedq(i_NumGroups,    &
                   i_NumMRVars,    &
                   i_NumQuantVals, &
                   da_QuantVals,   &
                   i_ActiveCases,  &
                   ia_GpSize,      &
                   iEr,ch_MyMsg)
!d USE debugmod, ONLY: MyDebug                                         ! debug!
!___Imported Parameters and Variables:
USE blcmnmod, ONLY: d_ONE, d_ZERO, i_NOT_OK, i_OK
        ! ^-- Some global Blossom variables and parameters.
USE ioformod, ONLY: ch12_OutBuff
        ! ^-- Output buffer variables.
USE jonsmodule, ONLY: ch_EM00202, ch_EM00203, ch_EM00204, ch_EM00205,         &
      ch_EM00206, ch_EM00207
        ! ^-- Message handling, etc.

USE mrolamod, ONLY: i_BIG_LIMIT, i_MAX_MR_VARS
        ! ^-- Permutation, regression and other Blossom stat parameters.
!___Imported Procedures:
USE jonsmodule, ONLY: errhand
       
IMPLICIT NONE
!___Dummy Arguments:
INTEGER, INTENT(IN)    :: i_NumGroups
INTEGER, INTENT(IN)    :: i_NumMRVars
INTEGER, INTENT(INOUT) :: i_NumQuantVals
REAL(KIND((0D0))), INTENT(INOUT) :: da_QuantVals(:)
INTEGER, INTENT(IN)    :: i_ActiveCases
INTEGER, INTENT(IN)    :: ia_GpSize(:)
INTEGER, INTENT(OUT)   :: iEr
INTEGER, PARAMETER :: i_LEN_MY_EMSG = 254 ! needs to be this big for output of var list in dostatus.
CHARACTER (LEN=i_LEN_MY_EMSG) :: ch_MyMsg
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     SUBROUTINE chkmedq
! DESCRIPTION:
!     Prepare to run the MEDQUANT program by checking contraints.
!     - quantiles range in value from 0.0 to 1.0
!     - number of quantiles:
!         Minimum 1
!         Maximum 2 billion
!     - number of variables:
!         Minimum 1
!         Maximum i_MAX_MR_VARS = 255
!     - number of cases:
!         Minimum 3 (2 will sometimes work).
!         Maximum 2 billion total cases.
!     - number of groups:
!         Minimum 1
!         Maximum 2 billion
!     - group sizes:
!         i.e., number of cases per group: (if not enough go on, but report?)
!         Minimum 3 (2 will sometimes work).
!         Maximum 2 billion total cases.
! LANGUAGE:
!     Fortran 90/95
! AUTHOR:
!     Jon D. Richards, Operations Research Analyst       jon_richards@usgs.gov
!     Fort Collins Science Center                     http://www.fort.usgs.gov
!     U.S. Geological Survey          -          Biological Resources Division
! INVOKED BY:
!     mqdoset        <File: "domedq.f90: mqdoset">         module
! INVOKES:
!     errhand        <File: "jonsmodule.f90: errhand">     module

!___Local Variables:
INTEGER :: i
!___Intrinsic Procedures:
INTRINSIC :: MAXVAL, MINVAL
!___Executable Statements:

iEr = i_OK ! no problem yet
IF (i_NumQuantVals < 1  .OR.  i_NumQuantVals > i_BIG_LIMIT) THEN
!_______________________________________________________________________
!                                                                       !
! "You must have more than 1 and fewer than 2147483647 quantile values" !
!_______________________________________________________________________!
!
   CALL errhand(ch_myMsg,ch_Msg=ch_EM00202)
   iEr = i_NOT_OK
   RETURN   ! ... error exit ...
END IF
DO i=1,i_NumQuantVals
   IF (da_QuantVals(i) < d_ZERO  .OR.  da_QuantVals(i) > d_ONE) THEN
!_______________________________________________
!                                               !
! "Quantile values must be between 0.0 and 1.0" !
!_______________________________________________!
!
      CALL errhand(ch_myMsg,ch_Msg=ch_EM00203)
      iEr = i_NOT_OK
      RETURN   ! ... error exit ...
   END IF
END DO

IF (i_ActiveCases < 3  .OR.  i_ActiveCases > i_BIG_LIMIT) THEN
!____________________________________________________________________
!                                                                    !
! "You must have more than 2 and fewer than 2147483647 observations" !
!____________________________________________________________________!
!
   CALL errhand(ch_myMsg,ch_Msg=ch_EM00205)
   iEr = i_NOT_OK
   RETURN   ! ... error exit ...
END IF
IF (l_IsGrouped) THEN
   IF (i_NumGroups < 1  .or.  i_NumGroups > i_BIG_LIMIT) THEN
!_______________________________________________________________________________
!                                                                               !
! "You must have more than 1 and fewer than 2147483647 groups for grouped data" !
!_______________________________________________________________________________!
!
      CALL errhand(ch_myMsg,ch_Msg=ch_EM00206)
      iEr = i_NOT_OK
      RETURN   ! ... error exit ...
   END IF
   IF (MINVAL(ia_GpSize(1:i_NumGroups)) < 3  .OR.     &
       MAXVAL(ia_GpSize(1:i_NumGroups)) > i_BIG_LIMIT) THEN
!_______________________________________________________________________________
!                                                                               !
! "You must have more than 2 and fewer than 2147483647 observations in a group" !
!_______________________________________________________________________________!
!
      CALL errhand(ch_myMsg,ch_Msg=ch_EM00207)
      iEr = i_NOT_OK
      RETURN   ! ... error exit ...
   END IF
END IF
RETURN   ! ... normal return ...
END SUBROUTINE chkmedq



!====================================================
SUBROUTINE medquant(i_NumVars,                &
                    i_NumGrps,                &
                    ia_GrpSizes,              &
                    i_NumQuantVals,			  &
                    da_QuantVals,			  &
                    da_Data,                  &
                    da_VariableWInGpMedian,   &
                    da_GpAvgDistToGpMVMedian, &
                    da_GpMedQTolerance,       &
                    da_WInGrpVarEstimateVal,  &
                    da_WInGpQuantDist,        &
                    da_ObsDistToGpMedian,     &
                    ia_NumIterations,         &
                    iEr,					  &
                    Ch_MyMsg)

!___Imported Parameters and Variables:
USE blcmnmod, ONLY: d_ONE, d_ZERO, i_NumCases, i_OK
        ! ^-- Some global Blossom variables and parameters.
USE ioformod, ONLY: ch25_OutBuff
        ! ^-- Buffers for character output of numbers.
USE jonsmodule, ONLY: i_CCode, i_MEDQMOD_ER001
        ! ^-- Message handling, etc.
!___Imported Procedures:

USE jonsmodule, ONLY:  hsort
        ! ^-- hsort: Sort a real (KIND(0D0)) array.
IMPLICIT NONE
!___Dummy Arguments:
INTEGER,          INTENT(IN)    :: i_NumVars
INTEGER,          INTENT(IN)    :: i_NumGrps
INTEGER,          INTENT(IN)    :: ia_GrpSizes(:)
INTEGER, 		  INTENT(IN)    :: i_NumQuantVals
REAL (KIND(0D0)),  INTENT(INOUT) 	:: da_QuantVals(:)
REAL (KIND(0D0)), INTENT(INOUT) :: da_Data(:,:)
REAL (KIND(0D0)), INTENT(OUT)   :: da_VariableWInGpMedian(:,:)
REAL (KIND(0D0)), INTENT(OUT)   :: da_GpAvgDistToGpMVMedian(:)
REAL (KIND(0D0)), INTENT(OUT)   :: da_GpMedQTolerance(:)
REAL (KIND(0D0)), INTENT(OUT)   :: da_WInGrpVarEstimateVal(:,:)
REAL (KIND(0D0)), INTENT(OUT)   :: da_WInGpQuantDist(:,:)
REAL (KIND(0D0)), INTENT(OUT)   :: da_ObsDistToGpMedian(:,:)
INTEGER,          INTENT(OUT)   :: ia_NumIterations(:)
INTEGER,          INTENT(OUT)   :: iEr
INTEGER, PARAMETER :: i_LEN_MY_EMSG = 254 ! needs to be this big for output of var list in dostatus.
CHARACTER (LEN=i_LEN_MY_EMSG) :: ch_MyMsg

! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     SUBROUTINE: medquant
! DESCRIPTION:
!     Driver calls RMEDQ1 and RMEDQ to compute medians and
!     quantiles for univariate and multivariate data sets, respectively.
!     This driver can read 1 to many groups of data that are sent to
!     RMEDQ1 or RMEDQ and the group median/quantiles/distances for each group
!     are calculated and written to the output filesS.
!           Input Array Dimensions:
!              ia_GrpSizes  (i_NumGrps)
!              da_Data      (iNumObvs,i_NumVars)
!           Output Array Dimensions:
!              da_VariableWInGpMedian    (i_NumGrps,i_NumVars)
!              da_GpAvgDistToGpMVMedian  (i_NumGrps)
!              da_WInGpQuantDist         (i_NumGrps,iNumQuan)
!              da_ObsDistToGpMedian      (i_NumGrps,MAX(ia_GrpSizes))
! LANGUAGE:
!     Fortran 90, with Lahey Computer Systems LF90 intrinsics extensions
! AUTHOR:
!     Jon D. Richards, Operations Research Analyst       jon_richards@usgs.gov
!     Fort Collins Science Center                     http://www.fort.usgs.gov
!     U.S. Geological Survey          -          Biological Resources Division
!     Subroutines RMEDQ1 and RMEDQ written by Dr. Paul W. Mileke, Jr.,
!     Statistics Department, Colorado State University.
! INVOKED BY:
!     runmedq       <File: "domedq.f90: runmedq">         module
! INVOKES:
!     hsort         <File: "jonsmodule.f90: hsort">       module
!     rmedq         <File: "medqmod.f90: rmedq">          module
!     rmedq1        <File: "medqmod.f90: rmedq1">         module

! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!___Local Variables:
INTEGER :: i
INTEGER :: j

INTEGER :: i_DX1, i_DX2
! INTEGER :: ios
! LOGICAL :: l_Had2All
!___Intrinsic Procedures:
INTRINSIC :: ANY
!___Executable Statements:

iEr = i_OK ! no problem yet
ia_NumIterations = 0


CALL hsort(da_QuantVals, & !* <File: "jonsmodule.f90: hsort">
           i_NumQuantVals)

! This should be done outside of this routine, but ...
! Check to see bounds are from within 0.0 to 1.0 only.
IF (ANY(da_QuantVals < d_ZERO)  .OR.  ANY(da_QuantVals > d_ONE)) THEN
!   ______________________________________________________________________________
!  |                                                                              |
!  | "Problem in MEDQUANT, You specified a quantile outside the range 0.0 to 1.0" |
!  |______________________________________________________________________________|
!
   iEr = i_MEDQMOD_ER001
   RETURN   !  ... error exit ... Specifications no good
END IF
i_DX2 = 0
DO i=1,i_NumGrps ! For each group, compute the quantiles.

   SELECT CASE (i_NumVars)
      CASE (i_UNIVARIATE)  ! Univariate.
         i_DX1 = i_DX2 + 1
         i_DX2 = i_DX1 + ia_GrpSizes(i) - 1
         CALL rmedq1(ia_GrpSizes(i),               &
         			 i_NumQuantVals,               &
                     da_Data(i_DX1:i_DX2,1),       &
                     da_QuantVals,                 &
                     da_VariableWInGpMedian(i,1),  &
                     da_GpAvgDistToGpMVMedian(i),  &
                     da_WInGrpVarEstimateVal(i,:), &
                     da_WInGpQuantDist(i,:),       &
                     da_ObsDistToGpMedian(i,:),    &
                     iEr,ch_MyMsg) !* <File: "medqmod.f90: rmedq1">
      CASE DEFAULT  ! Multivariate.
         i_DX1 = i_DX2 + 1
         i_DX2 = i_DX1 + ia_GrpSizes(i) - 1



         CALL rmedq(i_NumVars,                        &
                    ia_GrpSizes(i),                   &
                    i_NumQuantVals,                   &
                    da_Data(i_DX1:i_DX2,1:i_NumVars), &
                    da_QuantVals,                     &
                    da_VariableWInGpMedian(i,:),      &
                    da_GpAvgDistToGpMVMedian(i),      &
                    da_GpMedQTolerance(i),            &
                    da_WInGpQuantDist(i,:),           &
                    da_ObsDistToGpMedian(i,:),        &
                    ia_NumIterations(i),              &
                    iEr,ch_MyMsg) !* <File: "medqmod.f90: rmedq">
   END SELECT
   IF (iEr /= i_OK) EXIT ! If not going well, get out of loop.
call rchkusr()
END DO

RETURN   ! ... normal exit ...
END SUBROUTINE medquant

!=============================================================================!

SUBROUTINE rmedq1(i_NumObs,             &
				  i_NumQuantVals,       &
                  da_X1,                &
                  da_QuantVals,         &
                  dY_VariableMedian,    &
                  d_D_AvgDistToMedian,  &
                  da_VarEstimateVal,    &
                  da_QuantileDistance,  &
                  da_U_DistToMedian,    &
                  iEr,ch_MyMsg)
!___Imported Parameters and Variables:
USE blcmnmod, ONLY: d_HALF, d_ONE, d_TWO, d_ZERO, i_OK
        ! ^-- Some global Blossom variables and parameters.
USE jonsmodule, ONLY: i_CCode, i_MEDQMOD_ER002
        ! ^-- Message handling, etc.
!___Imported Procedures:
USE jonsmodule, ONLY: hsort, hsort2, hsorti2, lf_Equals
        ! ^-- hsort: Sort a real (KIND(0D0)) array.
        ! ^-- hsort2: Sort real (KIND(0D0)) array, keep integer array in order.
        ! ^-- hsorti2: Sort integer array, keep real (KIND(0D0)) array in order.
        ! ^-- lf_Equals: Generic test for equality of two objects.
IMPLICIT NONE
!___Dummy Arguments:
INTEGER,          INTENT(IN)    :: i_NumObs
INTEGER, 		  INTENT(IN)    :: i_NumQuantVals
REAL (KIND(0D0)), INTENT(INOUT) :: da_X1(:)
REAL (KIND(0D0)), INTENT(IN)    :: da_QuantVals(:)
REAL (KIND(0D0)), INTENT(OUT)   :: dY_VariableMedian
REAL (KIND(0D0)), INTENT(OUT)   :: d_D_AvgDistToMedian
REAL (KIND(0D0)), INTENT(OUT)   :: da_VarEstimateVal(:)
REAL (KIND(0D0)), INTENT(OUT)   :: da_QuantileDistance(:)
REAL (KIND(0D0)), INTENT(OUT)   :: da_U_DistToMedian(:)
INTEGER,          INTENT(OUT)   :: iEr
INTEGER, PARAMETER :: i_LEN_MY_EMSG = 254 ! needs to be this big for output of var list in dostatus.
CHARACTER (LEN=i_LEN_MY_EMSG) :: ch_MyMsg
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     SUBROUTINER medq1
! DESCRIPTION:
!     Selections of quantiles are obtained for a univariate set of data and
!     also distances of the same set from the median.
!     NOTE:  The univariate data set size must be at least 2.
!     U.S. Geological Survey          -          Biological Resources Division
!        Input Array Dimensions:
!           da_X1        (i_NumObs)
!           da_QuantVals (i_NumQuantVals)
!        Output Array Dimensions:
!           daVarEsimateVal    (i_NumQuantVals)
!           da_QuantileDistance (i_NumQuantVals)
!           da_U_DistToMedian   (i_NumObs)
! LANGUAGE:
!     Fortran 90/95
! AUTHOR:
!     Dr. Paul W. Mielke, Jr., Dept of Statistics, Colorado State University
!        http://www.stat.colostate.edu/~mielke/mielkesoftware.html
!     Modifications:
!        Jon D. Richards, Operations Research Analyst    jon_richards@usgs.gov
!        Fort Collins Science Center                  http://www.fort.usgs.gov
!        U.S. Geological Survey  -  Biological Resources Division
! INVOKED BY:
!     medquant  <File: "medqmod.f90: medquant">     module
! INVOKES:
!     hsort     <File: "jonsmodule.f90: hsort">     module
!     hsort2    <File: "jonsmodule.f90: hsort2">    module
!     hsorti2   <File: "jonsmodule.f90: hsorti2">   module
!     lf_Equals <File: "jonsmodule.f90: lf_Equals"> module

! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!___Local Variables:
REAL (KIND(0D0)), ALLOCATABLE :: da_V(:)
REAL (KIND(0D0)) :: d_G_VarMedianDist
INTEGER, ALLOCATABLE :: ia_OriOr(:)
INTEGER :: i, k, l
INTEGER :: ios
!___Intrinsic Procedures:
INTRINSIC :: ABS, DBLE, INT, MAX, MIN, NINT
!___Executable Statements:
iEr = i_OK ! no problem yet
ALLOCATE(                   &
          da_V(1:i_NumObs), &
      ia_OriOr(1:i_NumObs), &
      STAT=ios)
IF (ios /= i_OK) THEN
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |_________________
!  |                                                    |
!  | 'Memory allocation error: da_V,ia_OriOr in RMEDQ1' |
!  |____________________________________________________|
!
   iEr = i_MEDQMOD_ER002
   i_CCode = ios
   RETURN   ! ... error exit ...
END IF
da_U_DistToMedian = d_ZERO  ! Initialize array to zero.
da_V              = da_X1 ! Assign values of array da_X1 to array da_V.
CALL hsort(da_X1, &  !* <File: "jonsmodule.f90: hsort">
           i_NumObs) ! Sort array into ascending order.
! We will need median value below.
k = INT(d_HALF*DBLE(i_NumObs)+0.51D0)  ! Yes, 0.51D0
dY_VariableMedian = da_X1(k)  ! We will always need this below!
! Handle even number of observations variable median.
l = i_NumObs / 2
IF (k == l) dY_VariableMedian = (da_X1(k)+da_X1(k+1)) / d_TWO
da_VarEstimateVal = d_ZERO  ! Initialize array to zero.
DO i=1,i_NumQuantVals
   IF (lf_Equals(da_QuantVals(i), d_ZERO)) THEN ! Zero-th Quantile.
      da_VarEstimateVal(i) = da_X1(1)
   ELSE IF (lf_Equals(da_QuantVals(i), d_ONE)) THEN ! 1.0-th Quantile.
      da_VarEstimateVal(i) = da_X1(i_NumObs)
   ELSE IF (lf_Equals(da_QuantVals(i), d_HALF)) THEN ! Median.

      da_VarEstimateVal(i) = dY_VariableMedian
   ELSE
      IF (da_QuantVals(i) > d_HALF) THEN
         k = MIN(i_NumObs, NINT(da_QuantVals(i)*DBLE(i_NumObs)+d_HALF))
      ELSE
         k = MAX(1, NINT(da_QuantVals(i)*DBLE(i_NumObs)+d_HALF))
      END IF
      da_VarEstimateVal(i) = da_X1(k)
   END IF
END DO
d_D_AvgDistToMedian = d_ZERO
da_U_DistToMedian   = d_ZERO  ! Initialize array to zero.
DO i=1,i_NumObs ! da_U_DistToMedian(I) is distance from Ith point to median.
   da_U_DistToMedian(i) = ABS(da_V(i)-dY_VariableMedian)
   d_D_AvgDistToMedian  = d_D_AvgDistToMedian + da_U_DistToMedian(i)
END DO
d_D_AvgDistToMedian = d_D_AvgDistToMedian / DBLE(i_NumObs)
! We'll need the original order of the distance data.
ia_OriOr = (/ (i,i=1,i_NumObs) /)
CALL hsort2(da_U_DistToMedian, & !* <File: "jonsmodule.f90: hsort2">
            ia_OriOr,          &
            i_NumObs) ! Sort array.
! We'll want this variable's distance to median.
k = INT(d_HALF*DBLE(i_NumObs)+0.51D0)  ! Yes, 0.51D0
d_G_VarMedianDist = da_U_DistToMedian(k)
l = i_NumObs / 2
IF (k == l) d_G_VarMedianDist =                                         &
      (da_U_DistToMedian(k)+da_U_DistToMedian(k+1)) / d_TWO
da_QuantileDistance = d_ZERO
DO i=1,i_NumQuantVals
   IF (lf_Equals(da_QuantVals(i), d_ZERO)) THEN ! Zero-th Quantile.
      da_QuantileDistance(i) = da_U_DistToMedian(1)
   ELSE IF (lf_Equals(da_QuantVals(i), d_ONE)) THEN ! 1.0-th Quantile.
      da_QuantileDistance(i) = da_U_DistToMedian(i_NumObs)
   ELSE IF (lf_Equals(da_QuantVals(i), d_HALF)) THEN ! Median.

      da_QuantileDistance(i) = d_G_VarMedianDist
   ELSE
      IF (da_QuantVals(i) > d_HALF) THEN
         k = MIN(i_NumObs, NINT(da_QuantVals(i)*DBLE(i_NumObs)+d_HALF))
      ELSE
         k = MAX(1, NINT(da_QuantVals(i)*DBLE(i_NumObs)+d_HALF))
      END IF
      da_QuantileDistance(i) = da_U_DistToMedian(k)
   END IF
END DO
! Put distances back in original dataset order.
CALL hsorti2(da_U_DistToMedian, & !* <File: "jonsmodule.f90: hsorti2">
             ia_OriOr,          &
             i_NumObs) ! Sort array.
DEALLOCATE(ia_OriOr, da_V, STAT=ios)

RETURN   ! ... normal exit ...
END SUBROUTINE rmedq1

!=============================================================================!

SUBROUTINE rmedq(i_NumVars,                 &
                 i_NumObs,                  &
                 i_NumQuantVals, &
                 da_XM,                     &
                 da_QuantVals,              &
                 da_W2_VariableWInGpMedian, &
                 d_D_AvgDistToMedian,       &
                 d_MedQTolerance,           &
                 da_QuantileDistance,       &
                 da_U_DistToMedian,         &
                 i_NumIterations,           &
                 iEr,ch_MyMsg)
!___Imported Parameters and Variables:
USE blcmnmod, ONLY: d_HALF, d_ONE, d_TWO, d_ZERO, i_OK
        ! ^-- Some global Blossom variables and parameters.
USE jonsmodule, ONLY: i_CCode, i_MEDQMOD_ER003
        ! ^-- Message handling, etc.
!___Imported Procedures:
USE jonsmodule, ONLY: getepsz, hsort2, hsorti2, lf_Equals
        ! ^-- getepsz: Get a tolerance for a specified number's magnitude.
        ! ^-- hsort2: Sort a real (KIND(0D0)) array, keep integer array in order.
        ! ^-- hsorti2: Sort integer array, keep real (KIND(0D0)) array in order.
        ! ^-- lf_Equals: Generic test for equality of two objects.
IMPLICIT NONE
!___Dummy Arguments:
INTEGER,          INTENT(IN)    :: i_NumVars
INTEGER,          INTENT(IN)    :: i_NumObs
INTEGER, 		  INTENT(IN)    :: i_NumQuantVals
REAL (KIND(0D0)), INTENT(INOUT) :: da_XM(:,:)
REAL (KIND(0D0)), INTENT(IN)    :: da_QuantVals(:)
REAL (KIND(0D0)), INTENT(OUT)   :: da_W2_VariableWInGpMedian(:)
REAL (KIND(0D0)), INTENT(OUT)   :: d_D_AvgDistToMedian
REAL (KIND(0D0)), INTENT(OUT)   :: d_MedQTolerance
REAL (KIND(0D0)), INTENT(OUT)   :: da_QuantileDistance(:)
REAL (KIND(0D0)), INTENT(OUT)   :: da_U_DistToMedian(:)
INTEGER,          INTENT(OUT)   :: i_NumIterations
INTEGER,          INTENT(OUT)   :: iEr
INTEGER, PARAMETER :: i_LEN_MY_EMSG = 254 ! needs to be this big for output of var list in dostatus.
CHARACTER (LEN=i_LEN_MY_EMSG) :: ch_MyMsg
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     SUBROUTINE Rmedq
! DESCRIPTION:
!     Selections of quantiles are obtained for a multivariate set of data
!     and also distances of the same set from the median.
!     NOTE: R-dimensional median program requirement:   R >= 2
!        Input Array Dimensions:
!           da_XM        (i_NumObs,i_NumVars)
!           da_QuantVals (i_NumQuantVals)
!        Output Array Dimensions:
!           da_W2_VariableWInGpMedian  (i_NumVars)
!           da_QuantileDistance        (i_NumQuantVals)
!           da_U_DistToMedian          (i_NumObs)
! LANGUAGE:
!     Fortran 90/95
! AUTHOR:
!     Dr. Paul W. Mielke, Jr., Dept of Statistics, Colorado State University
!        http://www.stat.colostate.edu/~mielke/mielkesoftware.html
!     Modifications:
!        Jon D. Richards, Operations Research Analyst    jon_richards@usgs.gov
!        Fort Collins Science Center                  http://www.fort.usgs.gov
!        U.S. Geological Survey  -  Biological Resources Division
! INVOKED BY:
!     medquant  <File: "medqmod.f90: medquant">     module
! INVOKES:
!     getepsz   <File: "jonsmodule.f90: getepsz">   module
!     hsort2    <File: "jonsmodule.f90: hsort2">    module
!     hsorti2   <File: "jonsmodule.f90: hsorti2">   module
!     lf_Equals <File: "jonsmodule.f90: lf_Equals"> module

! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!___Local Variables:
REAL (KIND(0D0)), ALLOCATABLE :: da_W1(:)
REAL (KIND(0D0)) :: d_G_VarMedianDist
REAL (KIND(0D0)) :: d_MaxW1
REAL (KIND(0D0)) :: d_S
REAL (KIND(0D0)) :: d_V
REAL (KIND(0D0)) :: d_Y
REAL (KIND(0D0)) :: d_YM
REAL (KIND(0D0)) :: d_Z
INTEGER, ALLOCATABLE :: ia_OriOr(:)
INTEGER :: i, j, k, l, m
INTEGER :: ios
LOGICAL :: l_Flag
!___Intrinsic Procedures:
INTRINSIC :: ABS, DBLE, INT, MAX, MIN, NINT, SQRT, SUM
!___Executable Statements:

iEr = i_OK ! no problem yet

ALLOCATE(                    &
         da_W1(1:i_NumVars), &
      ia_OriOr(1:i_NumObs),  &
      STAT=ios)
IF (ios /= i_OK) THEN
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |_________________
!  |                                                    |
!  | 'Memory allocation error: da_W1,ia_OriOr in RMEDQ' |
!  |____________________________________________________|
!
   iEr = i_MEDQMOD_ER003
   i_CCode = ios
   RETURN   ! ... error exit ...
END IF
! Compute the Variable MEANS for this data.
   !  da_W1(:) = d_ZERO

!====================================
DO j=1,i_NumVars
   da_W1(j) = SUM(da_XM(1:i_NumObs,j))
   da_W1(j) = da_W1(j) / DBLE(i_NumObs)

END DO
!===================================

d_MaxW1 = SUM(da_W1(1:i_NumVars))/DBLE(i_NumVars)

d_MedQTolerance = getepsz(d_MaxW1, d_DEF_MEDQ_TOLERANCE) !* <File: "jonsmodule.f90: getepsz">

m = 0
DO
   m = m + 1
   IF (m > i_ITERATION_LIMIT) THEN
      i_NumIterations = m - 1
      EXIT ! exit the loop
   END IF
   DO j=1,i_NumVars
      d_V = d_ZERO
      d_Z = d_ZERO
      l_Flag = .FALSE.
      DO i=1,i_NumObs
         d_Y = SUM((da_XM(i,1:i_NumVars)-da_W1(1:i_NumVars))**2)
    
         IF (lf_Equals(d_Y, d_ZERO)) THEN

            CYCLE
         END IF
            d_V = d_V + da_XM(i,j)/SQRT(d_Y)
            d_Z = d_Z + d_ONE/SQRT(d_Y)

      END DO
      IF (l_Flag) CYCLE
      da_W2_VariableWInGpMedian(j) = d_V / d_Z

   END DO
   d_S = SUM(ABS(da_W1(1:i_NumVars)-da_W2_VariableWInGpMedian(1:i_NumVars)))
   IF (d_S < d_MedQTolerance) THEN
      i_NumIterations = m
      EXIT ! exit the loop
   END IF
   da_W1(1:i_NumVars) = da_W2_VariableWInGpMedian(1:i_NumVars)
END DO
d_D_AvgDistToMedian = d_ZERO
da_U_DistToMedian   = d_ZERO  ! Initialize array to zero.
DO i=1,i_NumObs
   d_YM = SUM((da_XM(i,1:i_NumVars)-   &
         da_W2_VariableWInGpMedian(1:i_NumVars))**2)
   d_YM = SQRT(d_YM)
   da_U_DistToMedian(i) = d_YM
   d_D_AvgDistToMedian  = d_D_AvgDistToMedian + d_YM
END DO
d_D_AvgDistToMedian = d_D_AvgDistToMedian / DBLE(i_NumObs)
! We'll need the original order of the distance data.
ia_OriOr = (/ (i,i=1,i_NumObs) /)
CALL hsort2(da_U_DistToMedian, & !* <File: "jonsmodule.f90: hsort2">
            ia_OriOr,          &
            i_NumObs) ! Sort the array.
! ////////////////////////////////////////////////
! DO we want to FORCE this value to ZERO????????
!  This is only true if the first distance is for the 0th quantile:
!  Zero if we didn't get there because of our tolerance:
IF (lf_Equals(da_QuantVals(1), d_ZERO)) THEN ! Zero-th Quantile.
   IF (da_U_DistToMedian(1) < d_MedQTolerance) THEN
!      da_U_DistToMedian(1) = 0.0D0
      da_U_DistToMedian(1) = d_ZERO
   END IF
END IF
! ////////////////////////////////////////////////
! We'll need this variable's distance to median.
k = INT(d_HALF*DBLE(i_NumObs)+0.51D0)  ! Yes, 0.51D0
d_G_VarMedianDist = da_U_DistToMedian(k)
l = i_NumObs / 2
IF (k == l) d_G_VarMedianDist =                                         &
      (da_U_DistToMedian(k)+da_U_DistToMedian(k+1)) / d_TWO
da_QuantileDistance = d_ZERO  ! Initialize array to zero.
DO i=1,i_NumQuantVals
   IF (lf_Equals(da_QuantVals(i), d_ZERO)) THEN ! Zero-th Quantile.
      ! DO we want to FORCE this value to ZERO? ???????
      da_QuantileDistance(i) = da_U_DistToMedian(1)
   ELSE IF (lf_Equals(da_QuantVals(i), d_ONE)) THEN ! 1.0-th Quantile.
      da_QuantileDistance(i) = da_U_DistToMedian(i_NumObs)
   ELSE IF (lf_Equals(da_QuantVals(i), d_HALF)) THEN ! Median.

      da_QuantileDistance(i) = d_G_VarMedianDist
   ELSE
      IF (da_QuantVals(i) > d_HALF) THEN
         k = MIN(i_NumObs, NINT(da_QuantVals(i)*DBLE(i_NumObs)+d_HALF))
      ELSE
         k = MAX(1, NINT(da_QuantVals(i)*DBLE(i_NumObs)+d_HALF))
      END IF
      da_QuantileDistance(i) = da_U_DistToMedian(k)
   END IF
END DO

!trying to fix 32 bit complie problem with values not comming across right
!  DO j=1,i_NumQuantVals
!        IF (da_W2_VariableWInGpMedian(j) < 0.0000000000001) THEN
!        da_W2_VariableWInGpMedian(j) = 0.0000000000001
!        END IF
!  END DO
      
! Put distances back in original order.
CALL hsorti2(da_U_DistToMedian, & !* <File: "jonsmodule.f90: hsorti2">
             ia_OriOr,          &
             i_NumObs) ! Sort array.
DEALLOCATE(ia_OriOr, da_W1, STAT=ios)

RETURN   ! ... normal exit ...
END SUBROUTINE rmedq

!=============================================================================!

END MODULE medqmod


!=============================================================================!
MODULE mrspmod
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     MODULE mrspmod
! DESCRIPTION
!     Perform Multi-Response Sequence Procedures (MRSP) or Exact MRSP.
! LANGUAGE:
!     Fortran 90/95
! AUTHOR:
!     Jon D. Richards, Operations Research Analyst       jon_richards@usgs.gov
!     Fort Collins Science Center                     http://www.fort.usgs.gov
!     U.S. Geological Survey          -          Biological Resources Division
! MODIFICATION HISTORY:
!     Module created - JDR  19 Oct 1999
IMPLICIT NONE
SAVE
CONTAINS
!     MRSP3        <File: "mrspmod.f90: mrsp3">
!        MRSPDIST  <File: "mrspmod.f90: mrspdist">
!     EMRSP        <File: "mrspmod.f90: emrsp">
!        EMRSPFACT <File: "mrspmod.f90: emrspfact">
!        EMRSPDVAL <File: "mrspmod.f90: emrspdval">

!=============================================================================!

SUBROUTINE mrsp3(i_NumObs,       &
                 i_NumVars,      &
                 i_NumPerms,     &
                 l_DoResample,   &
                 d_DistExpon,    &
                 da_Data,        &
                 d_TestStat,     &
                 d_ObsDelta,     &
                 d_ExpDelta,     &
                 d_VarDelta,     &
                 d_SkwDelta,     &
                 d_RhoAgreement, &
                 d_PValue,       &
                 iEr,l_SaveTest,da_STV,i_seed)
!___Imported Parameters and Variables:
!d USE debugmod, ONLY: MyDebug                                         ! debug!
USE blcmnmod, ONLY: d_ONE, d_ZERO, i_OK
        ! ^-- Some global Blossom variables and parameters.
USE jonsmodule, ONLY: i_CCode
        ! ^-- Message handling, etc.
!___Imported Procedures:
USE jonsmodule, ONLY: lf_Equals
        ! ^-- lf_Equals: Generic test for equality of two objects.
IMPLICIT NONE
!___Dummy Arguments:
INTEGER,          INTENT(IN)   :: i_NumObs
INTEGER,          INTENT(IN)   :: i_NumVars
INTEGER,          INTENT(IN)   :: i_NumPerms
LOGICAL,          INTENT(IN)   :: l_DoResample
REAL (KIND(0D0)), INTENT(IN)   :: d_DistExpon
REAL (KIND(0D0)), INTENT(INOUT):: da_Data(:,:)
REAL (KIND(0D0)), INTENT(OUT)  :: d_TestStat
REAL (KIND(0D0)), INTENT(OUT)  :: d_ObsDelta
REAL (KIND(0D0)), INTENT(OUT)  :: d_ExpDelta
REAL (KIND(0D0)), INTENT(OUT)  :: d_VarDelta
REAL (KIND(0D0)), INTENT(OUT)  :: d_SkwDelta
REAL (KIND(0D0)), INTENT(OUT)  :: d_RhoAgreement
REAL (KIND(0D0)), INTENT(OUT)  :: d_PValue
INTEGER,          INTENT(OUT)  :: iEr
REAL (KIND(0D0)), INTENT(OUT) :: da_STV(:)  ! save test values
INTEGER,          INTENT(INOUT)    :: i_seed
LOGICAL,  		   INTENT (IN)   :: l_SaveTest
!     SUBROUTINE mrsp3
! DESCRIPTION:
!     Multi-Response Sequence Permutation Procedures.
!     The test statistic associated with Multi-Response Sequence
!     Permutation Procedures (MRSP3) is calculated in this F77 ( now F90)
!     program.  This test is designed to analyze multi-response data
!     at the ordinal (or higher) levels and is based on the average
!     euclidean distance between adjacent points of an observed sequence
!     of points in an R-dimensional space (which correspond to R measured
!     responses on each object of an observed sequence of objects).
!     The present maximum values of N and R in this program are 200
!     and 20, respectively.  (now virtual sized)
!     N = i_NumObs and R = i_NumVars - JDR  19 Oct 1999
! LANGUAGE:
!     Fortran 90, with Lahey Computer Systems LF90 intrinsics extensions
! AUTHOR:
!     Dr. Paul W. Mielke, Jr., Dept of Statistics, Colorado State University
!        http://www.stat.colostate.edu/~mielke/mielkesoftware.html
!     Modifications:
!        Jon D. Richards, Operations Research Analyst    jon_richards@usgs.gov
!        Fort Collins Science Center                  http://www.fort.usgs.gov
!        U.S. Geological Survey  -  Biological Resources Division
! INVOKED BY:
!     runmrsp   <File: "domrsp.f90: runmrsp">       module
! INVOKES:
!     lf_Equals <File: "jonsmodule.f90: lf_Equals"> module
!     mrspdist  <File: "mrspmod.f90: mrspdist">     module

!___Intrinsic Procedures:
INTRINSIC :: HUGE
!___Executable Statements:

d_TestStat = HUGE(d_ONE)
d_ObsDelta = HUGE(d_ONE)
d_ExpDelta = HUGE(d_ONE)
d_VarDelta = HUGE(d_ONE)
d_TestStat = HUGE(d_ONE)
d_SkwDelta = HUGE(d_ONE)
d_PValue   = HUGE(d_ONE)
CALL mrspdist(i_NumObs,     & !* <File: "mrspmod.f90: mrspdist">
              i_NumVars,    &
              i_NumPerms,   &
              l_DoResample, &
              d_DistExpon,  &
              da_Data,      &
              d_ObsDelta,   &
              d_ExpDelta,   &
              d_VarDelta,   &
              d_TestStat,   &
              d_SkwDelta,   &
              d_PValue,     &
              iEr,l_SaveTest,da_STV,i_seed)
IF (iEr /= i_OK) THEN

   RETURN   ! ... error exit ...
END IF
IF (.NOT.lf_Equals(d_ExpDelta, d_ZERO)) THEN
   d_RhoAgreement = d_ONE - d_ObsDelta/d_ExpDelta
ELSE
   d_RhoAgreement = HUGE(d_ONE)
END IF

RETURN   ! ... normal exit ...
END SUBROUTINE mrsp3

!=============================================================================!

SUBROUTINE mrspdist(i_NumObs,     &
                    i_NumVars,    &
                    i_NumPerms,   &
                    l_DoResample, &
                    d_DistExpon,  &
                    da_Data,      &
                    d_ObsDelta,   &
                    d_ExpDelta,   &
                    d_VarDelta,   &
                    d_TestStat,   &
                    d_SkwDelta,   &
                    d_PValue,     &
                    iEr,l_SaveTest,da_STV,i_seed)
!___Imported Parameters and Variables:

USE blcmnmod, ONLY: d_ZERO, d_HALF, d_ONE, d_TWO, i_OK
        ! ^-- Some global Blossom variables and parameters.
USE jonsmodule, ONLY: i_CCode, i_MRSPMOD_ER002, i_MRSPMOD_ER003 !x, ch_STR0013
        ! ^-- Message handling, etc.
!___Imported Procedures:
USE mrppmod, ONLY: sampwor
        ! ^-- sampwor: sample from an array
USE pv_modul, ONLY: pvalue
        ! ^-- pvalue: compute P-value
IMPLICIT NONE
!___Dummy Arguments:
INTEGER,          INTENT(IN)    :: i_NumObs
INTEGER,          INTENT(IN)    :: i_NumVars
INTEGER,          INTENT(IN)    :: i_NumPerms
LOGICAL,          INTENT(IN)    :: l_DoResample
REAL (KIND(0D0)), INTENT(IN)    :: d_DistExpon
REAL (KIND(0D0)), INTENT(INOUT) :: da_Data(:,:)
REAL (KIND(0D0)), INTENT(OUT)   :: d_ObsDelta
REAL (KIND(0D0)), INTENT(OUT)   :: d_ExpDelta
REAL (KIND(0D0)), INTENT(OUT)   :: d_VarDelta
REAL (KIND(0D0)), INTENT(OUT)   :: d_TestStat
REAL (KIND(0D0)), INTENT(OUT)   :: d_SkwDelta
REAL (KIND(0D0)), INTENT(OUT)   :: d_PValue
INTEGER,          INTENT(OUT)   :: iEr
REAL (KIND(0D0)), INTENT(OUT) :: da_STV(:)  ! save test values
INTEGER,          INTENT(INOUT)    :: i_seed
LOGICAL,  		   INTENT (IN)   :: l_SaveTest
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     SUBROUTINE mrspdist
! DESCRIPTION:
!     calculates the distances between the data points, plus the parameters
!     which are directly dependent on the distances.
! LANGUAGE:
!     Fortran 90/95
! AUTHOR:
!     Dr. Paul W. Mielke, Jr., Dept of Statistics, Colorado State University
!        http://www.stat.colostate.edu/~mielke/mielkesoftware.html
!     Modifications:
!        Jon D. Richards, Operations Research Analyst    jon_richards@usgs.gov
!        Fort Collins Science Center                  http://www.fort.usgs.gov
!        U.S. Geological Survey  -  Biological Resources Division
! INVOKED BY:
!     mrsp3   <File: "mrspmod.f90: mrsp3">   module
! INVOKES:
!     pvalue  <File: "pv_modul.f90: pvalue"> module
!     sampwor <File: "mrppmod.f90: sampwor"> module

! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!___Procedure Parameters:
REAL (KIND(0D0)), PARAMETER :: d_PREC = 1.0D-12
!___Local Variables:
REAL (KIND(0D0)), ALLOCATABLE :: da_DC(:,:)
REAL (KIND(0D0)), ALLOCATABLE :: da_Del(:)  ! < this is now 1-D
REAL (KIND(0D0)) :: da_Q(6)
REAL (KIND(0D0)) :: d_A1, d_A2, d_A3, d_A4, d_A5, d_A6
REAL (KIND(0D0)) :: d_A7, d_A8, d_A9, d_A10, d_A11, d_A12
REAL (KIND(0D0)) :: d_B1, d_B2, d_B3, d_B4, d_B5, d_B6
REAL (KIND(0D0)) :: d_B7, d_B8, d_B9, d_B10, d_B11, d_B12
REAL (KIND(0D0)) :: d_D2, d_D3, d_D4, d_D5, d_D6
REAL (KIND(0D0)) :: d_D7, d_D8, d_D9, d_D10, d_D11, d_D12
REAL (KIND(0D0)) :: d_DX, d_DZ
REAL (KIND(0D0)) :: d_StdDev, d_Sum
INTEGER :: i, j, k
INTEGER :: ios, ios2
INTEGER :: IW
INTEGER :: i_MP
INTEGER :: m
INTEGER :: ix, ip, jp
LOGICAL :: l_DataToZero
!___Intrinsic Procedures:
INTRINSIC :: ALLOCATED, DBLE, SQRT, SUM, TRIM
!___Statement Function:
INTEGER dispos
dispos(i, j) = i + (2*i_NumObs-j)*(j-1)/2
!___Executable Statements:

iEr = i_OK ! no problem yet
d_ExpDelta = d_ZERO
d_VarDelta = d_ZERO
d_TestStat = d_ZERO
d_SkwDelta = d_ZERO
d_PValue   = d_ZERO
ALLOCATE(da_Del(1:i_NumObs+i_NumObs*(i_NumObs-1)/2), STAT=ios)
IF (ios /= i_OK) THEN
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |____________
!  |                                               |
!  | 'Memory allocation error: da_Del in MRSPDIST' |
!  |_______________________________________________|
!
   i_CCode = ios
   iEr = i_MRSPMOD_ER002

   RETURN   ! ... error exit ...
END IF

DO i=1,i_NumObs
   DO j=1,i_NumObs
      IF (j <= i) THEN
         ip = i
         jp = j
      ELSE
         ip = j
         jp = i
      END IF
      ix = dispos(ip, jp) ! ip+(2*i_NumObs-jp)*(jp-1)/2
      da_Del(ix) = SUM((da_Data(i,1:i_NumVars)-da_Data(j,1:i_NumVars))**2)
      da_Del(ix) = da_Del(ix)**(d_DistExpon/d_TWO)
   END DO
END DO
ALLOCATE(da_DC(1:3,1:i_NumObs), STAT=ios)
IF (ios /= i_OK) THEN
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |___________
!  |                                              |
!  | 'Memory allocation error: da_DC in MRSPDIST' |
!  |______________________________________________|
!
   i_CCode = ios
   iEr = i_MRSPMOD_ER003
   IF (ALLOCATED(da_DC))  DEALLOCATE(da_DC,  STAT=ios)
   IF (ALLOCATED(da_Del)) DEALLOCATE(da_Del, STAT=ios)

   RETURN   ! ... error exit ...
END IF
d_ObsDelta = d_ZERO
DO i=1,i_NumObs-1
   ! ix = dispos(i,i+1)
   ix = dispos(i+1, i)

   d_ObsDelta = d_ObsDelta + da_Del(ix)
END DO
d_ObsDelta = d_ObsDelta/DBLE(i_NumObs-1)
DO i=1,i_NumObs
   DO k=1,3
      da_DC(k,i) = d_ZERO
      DO j=1,i_NumObs
         IF (j <= i) THEN
            ip = i
            jp = j
         ELSE
            ip = j
            jp = i
         END IF
         ix = dispos(ip, jp) ! ip+(2*i_NumObs-jp)*(jp-1)/2
         ! da_DC(k,i) = da_DC(k,i) + da_Del(i,j)**k
         da_DC(k,i) = da_DC(k,i) + da_Del(ix)**k
         call rchkusr()
      END DO
   END DO
END DO
IF (l_DoResample) THEN

   d_DX = d_ObsDelta*(d_ONE+d_PREC)

IF (l_SAVETEST) THEN ! We want to save original and all resampled test statistics
      da_STV(1) = d_DX
END IF
   i_MP = 1
   l_DataToZero = .FALSE.
   DO IW=1,i_NumPerms-1
      CALL SAMPWOR(i_NumObs, i_NumObs, i_NumVars, da_Data, l_DataToZero)
      d_DZ = d_ZERO
      DO i=1,i_NumObs
         DO j=1,i_NumObs
            IF (j <= i) THEN
               ip = i
               jp = j
            ELSE
               ip = j
               jp = i
            END IF
            ix = dispos(ip, jp) ! ip+(2*i_NumObs-jp)*(jp-1)/2
            da_Del(ix) = d_ZERO
            DO k=1,i_NumVars
               da_Del(ix) = da_Del(ix) + (da_Data(i,k)-da_Data(j,k))**2
               call rchkusr()
            END DO
            da_Del(ix) = da_Del(ix)**(d_DistExpon/d_TWO)
         END DO
      END DO
      DO i=1,i_NumObs-1
         ix = dispos(i, i+1) ! ip+(2*i_NumObs-jp)*(jp-1)/2
         ! d_DZ = d_DZ + da_Del(i,i+1)
         d_DZ = d_DZ + da_Del(ix)
         call rchkusr()
      END DO
      d_DZ = d_DZ/(i_NumObs-1)
		IF (l_SAVETEST) THEN ! Save value of test stat for this permuted sample
         da_STV(IW+1) = d_DZ
      END IF
      IF (d_DZ < d_DX) i_MP = i_MP + 1
   END DO
   d_PValue = DBLE(i_MP)/DBLE(i_NumPerms)
        ! END P-Value from RESAMPLE estimate

ELSE
        ! BEGIN P-Value from PEARSON TYPE III distribution estimate
   d_B1  = d_ZERO
   d_B2  = d_ZERO
   d_B3  = d_ZERO
   d_B5  = d_ZERO
   d_B6  = d_ZERO
   d_B7  = d_ZERO
   d_B9  = d_ZERO
   d_B10 = d_ZERO
   DO i=1,i_NumObs
      d_B1  = d_B1  + da_DC(1,i)
      d_B2  = d_B2  + da_DC(2,i)
      d_B3  = d_B3  + da_DC(1,i)**2
      d_B5  = d_B5  + da_DC(3,i)
      d_B6  = d_B6  + da_DC(1,i)*da_DC(2,i)
      d_B10 = d_B10 + da_DC(1,i)**3
      call rchkusr()
   END DO
   DO i=1,i_NumObs-1
      DO j=i+1,i_NumObs
         IF (j <= i) THEN
            ip = i
            jp = j
         ELSE
            ip = j
            jp = i
         END IF
         ix = dispos(ip, jp) ! ip+(2*i_NumObs-jp)*(jp-1)/2
         ! d_B9 = d_B9 + da_DC(1,i)*da_DC(1,j)*da_Del(i,j)
         d_B9 = d_B9 + da_DC(1,i)*da_DC(1,j)*da_Del(ix)
         call rchkusr()
      END DO
   END DO
   DEALLOCATE(da_DC, STAT=ios2)
   d_B9 = d_B9*d_TWO
   d_Sum = d_ZERO
   DO i=1,i_NumObs-2
      DO j=i+1,i_NumObs-1
         DO k=j+1,i_NumObs
            ! d_Sum = da_Del(i,j)*da_Del(i,k)*da_Del(j,k)
            d_Sum = da_Del(j+(2*i_NumObs-i)*(i-1)/2)* &
                    da_Del(k+(2*i_NumObs-i)*(i-1)/2)* &
                    da_Del(k+(2*i_NumObs-j)*(j-1)/2)
            d_B7 = d_B7 + d_Sum
            call rchkusr()
         END DO
      END DO
   END DO
   DEALLOCATE(da_Del, STAT=ios2)
   d_B7  = d_B7*6.0D0
   d_B4  = d_B1**2
   d_B8  = d_B1*d_B2
   d_B11 = d_B1*d_B3
   d_B12 = d_B1**3
   d_A1  = d_B1
   d_A2  = d_B2
   d_A3  = d_B3  - d_B2
   d_A4  = d_B4  - d_A2*d_TWO - d_A3*4.0D0
   d_A5  = d_B5
   d_A6  = d_B6  - d_B5
   d_A7  = d_B7
   d_A8  = d_B8  + (d_B5-d_B6*d_TWO)*d_TWO
   d_A9  = d_B9  + d_B5 - d_B6*d_TWO - d_B7
   d_A10 = d_B10 + d_B5*d_TWO - d_B6*3.0D0
   d_A11 = d_B11 - (d_B10-d_B7+(d_B5+d_B9)*d_TWO)*d_TWO + d_B6*10.0D0 - d_B8
   d_A12 = d_B12 - (d_A5+(d_A7+d_A10+(d_A6+d_A9)*3.0D0)*d_TWO)*4.0D0 - (d_A8+d_A11*d_TWO)*6.0D0
   da_Q(1) = DBLE(i_NumObs)
   DO i=2,6
      da_Q(i) = da_Q(i-1)*DBLE(i_NumObs-i+1)
   END DO
   d_ExpDelta = d_A1/da_Q(2)
   d_D2  = d_A2/da_Q(2)
   d_D3  = d_A3/da_Q(3)
   d_D4  = d_A4/da_Q(4)
   d_D5  = d_A5/da_Q(2)
   d_D6  = d_A6/da_Q(3)
   d_D7  = d_A7/da_Q(3)
   d_D8  = d_A8/da_Q(4)
   d_D9  = d_A9/da_Q(4)
   d_D10 = d_A10/da_Q(4)
   d_D11 = d_A11/da_Q(5)
   d_D12 = d_A12/da_Q(6)
   m    = i_NumObs
   d_VarDelta = (                    &
         d_D2*DBLE(m-1) -            &
         d_D3*DBLE((m-2)*2) +        &
         d_D4*DBLE(m-3)              &
                ) *                  &
         DBLE(m*(m-2)) / da_Q(2)**2
   d_StdDev = SQRT(d_VarDelta)
   d_TestStat = (d_ObsDelta-d_ExpDelta)/d_StdDev

   d_SkwDelta = (                                      &
         d_D5*DBLE((m-1)*(m-2)) -                      &
         d_D6*DBLE((m-2)*(m-2)*6) +                    &
         d_D7*DBLE((m-2)*4) +                          &
         (d_D8*3.0D0+d_D10*4.0D0)*DBLE((m-2)*(m-3)) +  &
         (d_D9-d_D11*d_TWO)*DBLE((m-3)*(m-4)*6) +      &
         d_D12*DBLE((m-3)*(m-5)*4)                     &
                ) *                                    &
         DBLE(m*(m-4)) / (d_StdDev*da_Q(2))**3
   d_PValue = pvalue(d_TestStat, d_SkwDelta) !* <File: "pv_modul.f90: pvalue">

END IF
IF (ALLOCATED(da_DC))  DEALLOCATE(da_DC,  STAT=ios)
IF (ALLOCATED(da_Del)) DEALLOCATE(da_Del, STAT=ios)

RETURN   ! ... normal exit ...
END SUBROUTINE mrspdist

!=============================================================================!

SUBROUTINE mrsp32d(i_NumObs,       &
                 i_NumVars,      &
                 i_NumPerms,     &
                 l_DoResample,   &
                 d_DistExpon,    &
                 da_Datas,       &
                 d_TestStat,     &
                 d_ObsDelta,     &
                 d_ExpDelta,     &
                 d_VarDelta,     &
                 d_SkwDelta,     &
                 d_RhoAgreement, &
                 d_PValue,       &
                 iEr)
!___Imported Parameters and Variables:
USE blcmnmod, ONLY: d_ONE, d_ZERO, i_OK
        ! ^-- Some global Blossom variables and parameters.
USE jonsmodule, ONLY: i_CCode
        ! ^-- Message handling, etc.
!___Imported Procedures:
USE jonsmodule, ONLY: lf_Equals
        ! ^-- lf_Equals: Generic test for equality of two objects.
IMPLICIT NONE
!___Dummy Arguments:
INTEGER,          INTENT(IN)    :: i_NumObs
INTEGER,          INTENT(IN)    :: i_NumVars
INTEGER,          INTENT(IN)    :: i_NumPerms
LOGICAL,          INTENT(IN)    :: l_DoResample
REAL (KIND(0D0)), INTENT(IN)    :: d_DistExpon
REAL (KIND(0D0)), INTENT(INOUT) :: da_Datas(:,:)
REAL (KIND(0D0)), INTENT(OUT)   :: d_TestStat
REAL (KIND(0D0)), INTENT(OUT)   :: d_ObsDelta
REAL (KIND(0D0)), INTENT(OUT)   :: d_ExpDelta
REAL (KIND(0D0)), INTENT(OUT)   :: d_VarDelta
REAL (KIND(0D0)), INTENT(OUT)   :: d_SkwDelta
REAL (KIND(0D0)), INTENT(OUT)   :: d_RhoAgreement
REAL (KIND(0D0)), INTENT(OUT)   :: d_PValue
INTEGER,          INTENT(OUT)   :: iEr
!     SUBROUTINE mrsp3
! DESCRIPTION:
!     Multi-Response Sequence Permutation Procedures.
!     The test statistic associated with Multi-Response Sequence
!     Permutation Procedures (MRSP3) is calculated in this F77 ( now F90)
!     program.  This test is designed to analyze multi-response data
!     at the ordinal (or higher) levels and is based on the average
!     euclidean distance between adjacent points of an observed sequence
!     of points in an R-dimensional space (which correspond to R measured
!     responses on each object of an observed sequence of objects).
!     The present maximum values of N and R in this program are 200
!     and 20, respectively.  (now virtual sized)
!     N = i_NumObs and R = i_NumVars - JDR  19 Oct 1999
! LANGUAGE:
!     Fortran 90, with Lahey Computer Systems LF90 intrinsics extensions
! AUTHOR:
!     Dr. Paul W. Mielke, Jr., Dept of Statistics, Colorado State University
!        http://www.stat.colostate.edu/~mielke/mielkesoftware.html
!     Modifications:
!        Jon D. Richards, Operations Research Analyst    jon_richards@usgs.gov
!        Fort Collins Science Center                  http://www.fort.usgs.gov
!        U.S. Geological Survey  -  Biological Resources Division
! INVOKED BY:
!     runmrsp   <File: "domrsp.f90: runmrsp">       module
! INVOKES:
!     lf_Equals <File: "jonsmodule.f90: lf_Equals"> module
!     mrspdist  <File: "mrspmod.f90: mrspdist">     module

! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!___Local Variables:
!___Intrinsic Procedures:
INTRINSIC :: HUGE
!___Executable Statements:
iEr = i_OK ! no problem yet
CALL mrspdist2d(i_NumObs,     & !* <File: "mrspmod.f90: mrspdist">
              i_NumVars,    &
              i_NumPerms,   &
              l_DoResample, &
              d_DistExpon,  &
              da_Datas,     &
              d_ObsDelta,   &
              d_ExpDelta,   &
              d_VarDelta,   &
              d_TestStat,   &
              d_SkwDelta,   &
              d_PValue,     &
              iEr)
IF (iEr /= i_OK) THEN
   RETURN   ! ... error exit ...
END IF
IF (.NOT.lf_Equals(d_ExpDelta, d_ZERO)) THEN
   d_RhoAgreement = d_ONE - d_ObsDelta/d_ExpDelta
ELSE
   d_RhoAgreement = HUGE(d_ONE)
END IF
RETURN   ! ... normal exit ...
END SUBROUTINE mrsp32d

!=============================================================================!

SUBROUTINE mrspdist2d(i_NumObs,     &
                    i_NumVars,    &
                    i_NumPerms,   &
                    l_DoResample, &
                    d_DistExpon,  &
                    da_Datas,     &
                    d_ObsDelta,   &
                    d_ExpDelta,   &
                    d_VarDelta,   &
                    d_TestStat,   &
                    d_SkwDelta,   &
                    d_PValue,     &
                    iEr)
!___Imported Parameters and Variables:

USE blcmnmod, ONLY: d_ZERO, d_HALF, d_ONE, d_TWO, i_OK
        ! ^-- Some global Blossom variables and parameters.
USE jonsmodule, ONLY: i_CCode, i_MRSPMOD_ER002, i_MRSPMOD_ER003 !x, ch_STR0013
        ! ^-- Message handling, etc.
!___Imported Procedures:
USE mrppmod, ONLY: sampwor
        ! ^-- sampwor: sample from an array
USE pv_modul, ONLY: pvalue
        ! ^-- pvalue: compute P-value
IMPLICIT NONE
!___Dummy Arguments:
INTEGER,          INTENT(IN)    :: i_NumObs
INTEGER,          INTENT(IN)    :: i_NumVars
INTEGER,          INTENT(IN)    :: i_NumPerms
LOGICAL,          INTENT(IN)    :: l_DoResample
REAL (KIND(0D0)), INTENT(IN)    :: d_DistExpon
REAL (KIND(0D0)), INTENT(INOUT) :: da_Datas(:,:)
REAL (KIND(0D0)), INTENT(OUT)   :: d_ObsDelta
REAL (KIND(0D0)), INTENT(OUT)   :: d_ExpDelta
REAL (KIND(0D0)), INTENT(OUT)   :: d_VarDelta
REAL (KIND(0D0)), INTENT(OUT)   :: d_TestStat
REAL (KIND(0D0)), INTENT(OUT)   :: d_SkwDelta
REAL (KIND(0D0)), INTENT(OUT)   :: d_PValue
INTEGER,          INTENT(OUT)   :: iEr
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     SUBROUTINE mrspdist
! DESCRIPTION:
!     calculates the distances between the data points, plus the parameters
!     which are directly dependent on the distances.
! LANGUAGE:
!     Fortran 90/95
! AUTHOR:
!     Dr. Paul W. Mielke, Jr., Dept of Statistics, Colorado State University
!        http://www.stat.colostate.edu/~mielke/mielkesoftware.html
!     Modifications:
!        Jon D. Richards, Operations Research Analyst    jon_richards@usgs.gov
!        Fort Collins Science Center                  http://www.fort.usgs.gov
!        U.S. Geological Survey  -  Biological Resources Division
! INVOKED BY:
!     mrsp3   <File: "mrspmod.f90: mrsp3">   module
! INVOKES:
!     pvalue  <File: "pv_modul.f90: pvalue"> module
!     sampwor <File: "mrppmod.f90: sampwor"> module
! FILES:
!     -
! DEVICES:
!     -
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!___Procedure Parameters:
REAL (KIND(0D0)), PARAMETER :: d_PREC = 1.0D-12
!___Local Variables:
REAL (KIND(0D0)), ALLOCATABLE :: da_DC(:,:)
REAL (KIND(0D0)), ALLOCATABLE :: da_Del(:,:)
REAL (KIND(0D0)), ALLOCATABLE :: da_STV(:)  ! save test values
REAL (KIND(0D0)) :: da_Q(6)
REAL (KIND(0D0)) :: d_A1, d_A2, d_A3, d_A4, d_A5, d_A6
REAL (KIND(0D0)) :: d_A7, d_A8, d_A9, d_A10, d_A11, d_A12
REAL (KIND(0D0)) :: d_B1, d_B2, d_B3, d_B4, d_B5, d_B6
REAL (KIND(0D0)) :: d_B7, d_B8, d_B9, d_B10, d_B11, d_B12
REAL (KIND(0D0)) :: d_D2, d_D3, d_D4, d_D5, d_D6
REAL (KIND(0D0)) :: d_D7, d_D8, d_D9, d_D10, d_D11, d_D12
REAL (KIND(0D0)) :: d_DX, d_DZ
REAL (KIND(0D0)) :: d_StdDev, d_Sum
INTEGER :: i, j, k
INTEGER :: ios, ios2
INTEGER :: IW
INTEGER :: i_MP
INTEGER :: m
LOGICAL :: l_DataToZero
!___Intrinsic Procedures:
INTRINSIC :: ALLOCATED, DBLE, SQRT, SUM
!___Executable Statements:

iEr = i_OK ! no problem yet
d_ExpDelta = d_ZERO
d_VarDelta = d_ZERO
d_TestStat = d_ZERO
d_SkwDelta = d_ZERO
d_PValue   = d_ZERO
ALLOCATE(da_Del(1:i_NumObs,1:i_NumObs), STAT=ios)
IF (ios /= i_OK) THEN
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |____________
!  |                                               |
!  | 'Memory allocation error: da_Del in MRSPDIST' |
!  |_______________________________________________|
!
   i_CCode = ios
   iEr = i_MRSPMOD_ER002
   RETURN   ! ... error exit ...
END IF

DO i=1,i_NumObs
   DO j=1,i_NumObs
      da_Del(i,j) = SUM((da_Datas(i,1:i_NumVars)-da_Datas(j,1:i_NumVars))**2)
      da_Del(i,j) = da_Del(i,j)**(d_DistExpon/d_TWO)
      call rchkusr()
   END DO
END DO
ALLOCATE(da_DC(1:3,1:i_NumObs), STAT=ios)
IF (ios /= i_OK) THEN
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |___________
!  |                                              |
!  | 'Memory allocation error: da_DC in MRSPDIST' |
!  |______________________________________________|
!
   i_CCode = ios
   iEr = i_MRSPMOD_ER003
   IF (ALLOCATED(da_DC))  DEALLOCATE(da_DC,  STAT=ios)
   IF (ALLOCATED(da_Del)) DEALLOCATE(da_Del, STAT=ios)
   RETURN   ! ... error exit ...
END IF
d_ObsDelta = d_ZERO
DO i=1,i_NumObs-1
   d_ObsDelta = d_ObsDelta + da_Del(i,i+1)
END DO
d_ObsDelta = d_ObsDelta/DBLE(i_NumObs-1)
DO i=1,i_NumObs
   DO k=1,3
      da_DC(k,i) = d_ZERO
      DO j=1,i_NumObs
         da_DC(k,i) = da_DC(k,i) + da_Del(i,j)**k
         call rchkusr()
      END DO
   END DO
END DO
IF (l_DoResample) THEN

   d_DX = d_ObsDelta*(d_ONE+d_PREC)

   i_MP = 1
   l_DataToZero = .FALSE.
   DO IW=1,i_NumPerms-1
      CALL SAMPWOR(i_NumObs,   & !* <File: "mrppmod.f90: sampwor">
                   i_NumObs,   &
                   i_NumVars,  &
                   da_Datas,   &
                   l_DataToZero)
      d_DZ = d_ZERO
      DO i=1,i_NumObs
         DO j=1,i_NumObs
            da_Del(i,j) = d_ZERO
            DO k=1,i_NumVars
               da_Del(i,j) = da_Del(i,j) + (da_Datas(i,k)-da_Datas(j,k))**2
            END DO
            da_Del(i,j) = da_Del(i,j)**(d_DistExpon/d_TWO)
            call rchkusr()
         END DO
      END DO
      DO i=1,i_NumObs-1
         d_DZ = d_DZ + da_Del(i,i+1)
      END DO
      d_DZ = d_DZ/(i_NumObs-1)

      IF (d_DZ < d_DX) i_MP = i_MP + 1
   END DO
   d_PValue = DBLE(i_MP)/DBLE(i_NumPerms)
   ! END P-Value from RESAMPLE estimate

! /////////////////////////////////////////////////////////////////////////////////////////////
!
! Check to see that we have access to SAVETEST file
!
! /////////////////////////////////////////////////////////////////////////////////////////////



ELSE
   ! BEGIN P-Value from PEARSON TYPE III distribution estimate
   d_B1  = d_ZERO
   d_B2  = d_ZERO
   d_B3  = d_ZERO
   d_B5  = d_ZERO
   d_B6  = d_ZERO
   d_B7  = d_ZERO
   d_B9  = d_ZERO
   d_B10 = d_ZERO
   DO i=1,i_NumObs
      d_B1  = d_B1  + da_DC(1,i)
      d_B2  = d_B2  + da_DC(2,i)
      d_B3  = d_B3  + da_DC(1,i)**2
      d_B5  = d_B5  + da_DC(3,i)
      d_B6  = d_B6  + da_DC(1,i)*da_DC(2,i)
      d_B10 = d_B10 + da_DC(1,i)**3
      call rchkusr()
   END DO
   DO i=1,i_NumObs-1
      DO j=i+1,i_NumObs
         d_B9 = d_B9 + da_DC(1,i)*da_DC(1,j)*da_Del(i,j)
         call rchkusr()
      END DO
   END DO
   DEALLOCATE(da_DC, STAT=ios2)
   d_B9 = d_B9*d_TWO
   d_Sum = d_ZERO
   DO i=1,i_NumObs-2
      DO j=i+1,i_NumObs-1
         DO k=j+1,i_NumObs
            d_Sum = da_Del(i,j)*da_Del(i,k)*da_Del(j,k)
            d_B7 = d_B7 + d_Sum
            call rchkusr()
         END DO
      END DO
   END DO
   DEALLOCATE(da_Del, STAT=ios2)
   d_B7  = d_B7*6.0D0
   d_B4  = d_B1**2
   d_B8  = d_B1*d_B2
   d_B11 = d_B1*d_B3
   d_B12 = d_B1**3
   d_A1  = d_B1
   d_A2  = d_B2
   d_A3  = d_B3  - d_B2
   d_A4  = d_B4  - d_A2*d_TWO - d_A3*4.0D0
   d_A5  = d_B5
   d_A6  = d_B6  - d_B5
   d_A7  = d_B7
   d_A8  = d_B8  + (d_B5-d_B6*d_TWO)*d_TWO
   d_A9  = d_B9  + d_B5 - d_B6*d_TWO - d_B7
   d_A10 = d_B10 + d_B5*d_TWO - d_B6*3.0D0
   d_A11 = d_B11 - (d_B10-d_B7+(d_B5+d_B9)*d_TWO)*d_TWO + d_B6*10.0D0 - d_B8
   d_A12 = d_B12 - (d_A5+(d_A7+d_A10+(d_A6+d_A9)*3.0D0)*d_TWO)*4.0D0 -        &
         (d_A8+d_A11*d_TWO)*6.0D0
   da_Q(1) = DBLE(i_NumObs)
   DO i=2,6
      da_Q(i) = da_Q(i-1)*DBLE(i_NumObs-i+1)
   END DO
   d_ExpDelta = d_A1/da_Q(2)
   d_D2  = d_A2/da_Q(2)
   d_D3  = d_A3/da_Q(3)
   d_D4  = d_A4/da_Q(4)
   d_D5  = d_A5/da_Q(2)
   d_D6  = d_A6/da_Q(3)
   d_D7  = d_A7/da_Q(3)
   d_D8  = d_A8/da_Q(4)
   d_D9  = d_A9/da_Q(4)
   d_D10 = d_A10/da_Q(4)
   d_D11 = d_A11/da_Q(5)
   d_D12 = d_A12/da_Q(6)
   m    = i_NumObs
   d_VarDelta = (                    &
         d_D2*DBLE(m-1) -            &
         d_D3*DBLE((m-2)*2) +        &
         d_D4*DBLE(m-3)              &
         ) *                         &
         DBLE(m*(m-2)) / da_Q(2)**2
   d_StdDev = SQRT(d_VarDelta)
   d_TestStat = (d_ObsDelta-d_ExpDelta)/d_StdDev

   d_SkwDelta = (                                      &
         d_D5*DBLE((m-1)*(m-2)) -                      &
         d_D6*DBLE((m-2)*(m-2)*6) +                    &
         d_D7*DBLE((m-2)*4) +                          &
         (d_D8*3.0D0+d_D10*4.0D0)*DBLE((m-2)*(m-3)) +  &
         (d_D9-d_D11*d_TWO)*DBLE((m-3)*(m-4)*6) +      &
         d_D12*DBLE((m-3)*(m-5)*4)                     &
           ) *                                         &
         DBLE(m*(m-4)) / (d_StdDev*da_Q(2))**3
   d_PValue = pvalue(d_TestStat, d_SkwDelta) !* <File: "pv_modul.f90: pvalue">
   ! END P-Value from PEARSON TYPE III distribution estimate
END IF
IF (ALLOCATED(da_DC))  DEALLOCATE(da_DC,  STAT=ios)
IF (ALLOCATED(da_Del)) DEALLOCATE(da_Del, STAT=ios)
RETURN   ! ... normal exit ...
END SUBROUTINE mrspdist2d

!=============================================================================!

SUBROUTINE emrsp(IC,          &
                 i_NumObs,    &
                 i_NumVars,   &
                 d_DistExpon, &
                 da_Datas,    &
                 d_ObsDelta,  &
                 d_PValue,    &
                 i_Factorial, &
                 iEr)
!___Imported Parameters and Variables:
USE blcmnmod, ONLY: d_ONE, d_TWO, d_ZERO, i_OK, l_TRUE
        ! ^-- Some global Blossom variables and parameters.
USE jonsmodule, ONLY: i_CCode, i_MRSPMOD_ER004
        ! ^-- Message handling, etc.
!___Imported Procedures:
!__(none)__
IMPLICIT NONE
!___Dummy Arguments:
        ! If IC = 1 then commensuration by average distance function.
INTEGER,          INTENT(INOUT) :: IC
INTEGER,          INTENT(IN)    :: i_NumObs
INTEGER,          INTENT(IN)    :: i_NumVars
REAL (KIND(0D0)), INTENT(IN)    :: d_DistExpon
REAL (KIND(0D0)), INTENT(INOUT) :: da_Datas(:,:)
REAL (KIND(0D0)), INTENT(OUT)   :: d_ObsDelta
REAL (KIND(0D0)), INTENT(OUT)   :: d_PValue
INTEGER,          INTENT(OUT)   :: i_Factorial
INTEGER,          INTENT(OUT)   :: iEr
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     SUBROUTINE emrsp
! DESCRIPTION:
!     Exact Multi-Response Sequence Procedures
!     This code was written by Kenneth J. Berry and Paul W. Mielke, Jr.
!                            Department of Statistics
!                            Colorado State University
!                            Fort Collins, Colorado 80523
! LANGUAGE:
!     Fortran 90, with Lahey Computer Systems LF90 intrinsics extensions
! AUTHOR:
!     Dr. Paul W. Mielke, Jr., Dept of Statistics, Colorado State University
!        http://www.stat.colostate.edu/~mielke/mielkesoftware.html
!     Modifications:
!        Jon D. Richards, Operations Research Analyst    jon_richards@usgs.gov
!        Fort Collins Science Center                  http://www.fort.usgs.gov
!        U.S. Geological Survey  -  Biological Resources Division
! INVOKED BY:
!     runmrsp   <File: "domrsp.f90: runmrsp">    module
! INVOKES:
!     emrspfact <File: "mrspmod.f90: emrspfact"> module

! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!___Procedure Parameters:
REAL (KIND(0D0)), PARAMETER :: d_PREC = 1.0D-12
!___Local Variables:
REAL (KIND(0D0)), ALLOCATABLE :: da_AD(:)   ! dim(i_NumVars)
REAL (KIND(0D0)) :: d_NumObsM1, d_Delta1, d_Sum
INTEGER :: i, j, k
INTEGER :: ios
INTEGER :: i_Kount1, i_Kount2
INTEGER :: i_NumObsM1
!___Intrinsic Procedures:
INTRINSIC :: ABS, DBLE, SUM
!___Executable Statements:

iEr = i_OK ! no problem yet
i_NumObsM1 = i_NumObs - 1
ALLOCATE(da_AD(1:i_NumVars), STAT=ios)
IF (ios /= i_OK) THEN
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |_____________
!  |                                                |
!  | 'Memory allocation error: da_AD,da_S in EMRSP' |
!  |________________________________________________|
!
   i_CCode = ios
   iEr = i_MRSPMOD_ER004
   RETURN   ! ... error exit ...
END IF
IF (i_NumVars == 1) IC = 0
IF (.NOT.(i_NumVars == 1  .OR.  IC /= 1)) THEN
   DO i=1,i_NumVars
      DO j=1,i_NumObsM1
         da_AD(i) =  &
               SUM((ABS(da_Datas(j,i)-da_Datas(j+1:i_NumObs,i)))**d_DistExpon)
      END DO
      da_AD(i) = da_AD(i)**(d_ONE/d_DistExpon)
   END DO
   DO i=1,i_NumObs
      da_Datas(i,1:i_NumVars) = da_Datas(i,1:i_NumVars)/da_AD(1:i_NumVars)
   END DO
END IF
d_Delta1 = d_ZERO
d_NumObsM1 = DBLE(i_NumObsM1)
DO i=1,i_NumObsM1
   d_Sum = d_ZERO
   DO k=1,i_NumVars
      d_Sum = d_Sum + (da_Datas(i,k)-da_Datas(i+1,k))**2
   END DO
   !.. d_Sum    = SUM((da_Datas(I,1:i_NumVars)-da_Datas(I+1,1:i_NumVars))**2)
   d_Delta1 = d_Delta1 + d_Sum**(d_DistExpon/d_TWO)
END DO
d_Delta1 = d_Delta1*(d_ONE+d_PREC)/d_NumObsM1
CALL emrspfact(i_NumObs,    & !* <File: "mrspmod.f90: emrspfact">
               i_NumVars,   &
               i_Kount1,    &
               i_Kount2,    &
               d_Delta1,    &
               d_NumObsM1,  &
               d_DistExpon, &
               da_Datas,    &
               iEr)
IF (iEr /= 0) THEN
   DEALLOCATE(da_AD, STAT=ios)
   RETURN   ! ... error exit ...
END IF
d_ObsDelta = d_Delta1
d_PValue = DBLE(i_Kount2)/DBLE(i_Kount1)
i_Factorial = i_Kount1
DEALLOCATE(da_AD, STAT=ios)
RETURN   ! ... normal exit ...
END SUBROUTINE emrsp

!=============================================================================!

SUBROUTINE emrspfact(i_NumObs,    &
                     i_NumVars,   &
                     i_Kount1,    &
                     i_Kount2,    &
                     d_Delta1,    &
                     d_NumObsM1,  &
                     d_DistExpon, &
                     da_Datas,    &
                     iEr)
!___Imported Parameters and Variables:
USE blcmnmod, ONLY: i_OK
        ! ^-- Some global Blossom variables and parameters.
USE jonsmodule, ONLY: i_CCode, i_MRSPMOD_ER006
        ! ^-- Message handling, etc.
!___Imported Procedures:
!__(none)__
IMPLICIT NONE
!___Dummy Arguments:
INTEGER,          INTENT(IN)  :: i_NumObs
INTEGER,          INTENT(IN)  :: i_NumVars
INTEGER,          INTENT(OUT) :: i_Kount1
INTEGER,          INTENT(OUT) :: i_Kount2
REAL (KIND(0D0)), INTENT(IN)  :: d_Delta1
REAL (KIND(0D0)), INTENT(IN)  :: d_NumObsM1
REAL (KIND(0D0)), INTENT(IN)  :: d_DistExpon
REAL (KIND(0D0)), INTENT(IN)  :: da_Datas(:,:)
        !         dim(i_NumObs,i_NumVars)--^
INTEGER,          INTENT(OUT) :: iEr
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     SUBROUTINE emrspfact
! DESCRIPTION:
!     Computations for Exact Multi-Response Sequence Procedures program.
! LANGUAGE:
!     Fortran 90/95
! AUTHOR:
!     Dr. Paul W. Mielke, Jr., Dept of Statistics, Colorado State University
!        http://www.stat.colostate.edu/~mielke/mielkesoftware.html
!     Modifications:
!        Jon D. Richards, Operations Research Analyst    jon_richards@usgs.gov
!        Fort Collins Science Center                  http://www.fort.usgs.gov
!        U.S. Geological Survey  -  Biological Resources Division
! INVOKED BY:
!     emrsp     <File: "mrspmod.f90: emrsp">     module
! INVOKES:
!     emrspdval <File: "mrspmod.f90: emrspdval"> module

!___Local Variables:
! REAL (KIND(0D0)) :: d_JM
INTEGER, ALLOCATABLE :: ia_A(:,:) ! dim(i_NumObs,i_NumObs)
!//////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
!  Mod ia_A to vectors ?
!\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\//////////////////////////////////
INTEGER, ALLOCATABLE :: ia_P(:) ! dim(i_NumObs)
INTEGER :: i, j, k, l, m, ii, jj, LL
INTEGER :: ios
INTEGER :: i_Flag, i_Limit, i_NMarks, i_Temp
!___Intrinsic Procedures:
!__(none)__
!___Executable Statements:

iEr = i_OK ! no problem yet
i_Kount1 = 0
i_Kount2 = 0
ALLOCATE(                          &
      ia_P(1:i_NumObs),            &
      ia_A(1:i_NumObs,1:i_NumObs), &
      STAT=ios)
IF (ios /= i_OK) THEN
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |_________________
!  |                                                    |
!  | 'Memory allocation error: ia_P, ia_A in EMRSPFACT' |
!  |____________________________________________________|
!
   i_CCode = ios
   iEr = i_MRSPMOD_ER006
   RETURN   ! ... error exit ...
END IF
k = i_NumObs
l = i_NumObs
DO i=1,k-1
   ia_A(i,1) = i
   DO j=2,l
      ia_A(i,j) = i + 1
   END DO
   l = l - 1
END DO
DO
   i = k - 1
   i_NMarks = 2
   IF (i_Kount1 == 0) GOTO 20
   loop10: DO
      DO j=1,i_NMarks-1
         IF (ia_A(i,j) == i) THEN
            IF (ia_A(i,j+1) == i+1) THEN
               i_Limit     = j - 2
               ia_A(i,j)   = ia_A(i,j+1)
               ia_A(i,j+1) = ia_A(i,j) - 1
               IF (i_Limit <= 0) GOTO 20
        !                GOTO 10
               EXIT loop10
            END IF
         END IF
      END DO
      IF (i_NMarks /= 1) THEN
         DO j=1,i_NMarks/2
            i_Temp               = ia_A(i,j)
            ia_A(i,j)            = ia_A(i,i_NMarks-j+1)
            ia_A(i,i_NMarks-j+1) = i_Temp
         END DO
      END IF
      i = i - 1
      IF (i == 0) THEN
         DEALLOCATE(ia_P, ia_A, STAT=ios)
         RETURN   ! ... normal exit ...
      END IF
      i_NMarks = i_NMarks + 1
   END DO loop10
        ! 10    CONTINUE
   loop10a: DO
      i_Flag = 0
      DO j=1,i_Limit
         IF (ia_A(i,j) == i+1) THEN
            IF (ia_A(i,j+1) == i) THEN
               ia_A(i,j)   = ia_A(i,j+1)
               ia_A(i,j+1) = ia_A(i,j) + 1
               i_Flag      = 1
            END IF
         END IF
      END DO
      IF (i_Flag == 1) THEN
         CYCLE
      ELSE
         EXIT
      END IF
   END DO loop10a
20    CONTINUE
   ia_A(k,1:i_NumObs) = ia_A(1,1:i_NumObs)
   IF (k /= 2) THEN
      DO l=2,k-1
         m = 1
         DO j=1,i_NumObs
            IF (ia_A(k,j) == l) THEN
               ia_A(k,j) = ia_A(l,m)
               m         = m + 1
            END IF
         END DO
      END DO
   END IF
   LL = 1
   DO ii=1,k
      DO jj=1,i_NumObs
         IF (ia_A(k,jj) == ii) THEN
            ia_P(LL) = jj
            LL       = LL + 1
         END IF
      END DO
   END DO
   CALL emrspdval(i_NumObs,    & !* <File: "mrspmod.f90: emrspdval">
                  i_NumVars,   &
                  i_Kount2,    &
                  d_Delta1,    &
                  d_NumObsM1,  &
                  d_DistExpon, &
                  ia_P,        &
                  da_Datas,    &
                  iEr)
   IF (iEr /= i_OK) THEN
      DEALLOCATE(ia_P, ia_A, STAT=ios)
      RETURN   ! ... error exit ...
   END IF
   i_Kount1 = i_Kount1 + 1
END DO
END SUBROUTINE emrspfact

!=============================================================================!

SUBROUTINE emrspdval(i_NumObs,    &
                     i_NumVars,   &
                     i_Kount2,    &
                     d_Delta1,    &
                     d_NumObsM1,  &
                     d_DistExpon, &
                     ia_P,        &
                     da_Datas,    &
                     iEr)
!___Imported Parameters and Variables:
USE blcmnmod, ONLY: d_ONE, d_TWO, d_ZERO, i_OK
        ! ^-- Some global Blossom variables and parameters.
USE jonsmodule, ONLY: i_CCode, i_MRSPMOD_ER007
        ! ^-- Message handling, etc.
!___Imported Procedures:
!__(none)__
IMPLICIT NONE
!___Dummy Arguments:
INTEGER,          INTENT(IN)    :: i_NumObs
INTEGER,          INTENT(IN)    :: i_NumVars
INTEGER,          INTENT(INOUT) :: i_Kount2
REAL (KIND(0D0)), INTENT(IN)    :: d_Delta1
REAL (KIND(0D0)), INTENT(IN)    :: d_NumObsM1
REAL (KIND(0D0)), INTENT(IN)    :: d_DistExpon
INTEGER,          INTENT(IN)    :: ia_P(:)    ! dim(i_NumObs)
REAL (KIND(0D0)), INTENT(IN)    :: da_Datas(:,:)
        !          dim(i_NumObs,i_NumVars) --^
INTEGER,          INTENT(OUT)   :: iEr
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     SUBROUTINE emrspdval
! DESCRIPTION:
!     Computations for Exact Multi-Response Sequence Procedures program.
! LANGUAGE:
!     Fortran 90/95
! AUTHOR:
!     Dr. Paul W. Mielke, Jr., Dept of Statistics, Colorado State University
!        http://www.stat.colostate.edu/~mielke/mielkesoftware.html
!     Modifications:
!        Jon D. Richards, Operations Research Analyst    jon_richards@usgs.gov
!        Fort Collins Science Center                  http://www.fort.usgs.gov
!        U.S. Geological Survey  -  Biological Resources Division
! INVOKED BY:
!     emrsp <File: "mrspmod.f90: emrspfact"> module
! INVOKES:
!     -

! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!___Local Variables:
REAL (KIND(0D0)), ALLOCATABLE :: da_X(:,:) ! dim(i_NumObs,i_NumVars)
REAL (KIND(0D0)) :: d_Delta, d_Sum
INTEGER :: i, k
INTEGER :: ios
INTEGER :: i_NumObsM1
!___Intrinsic Procedures:
! INTRINSIC SUM
!___Executable Statements:
iEr = i_OK ! no problem yet
i_NumObsM1 = i_NumObs - 1
ALLOCATE(da_X(1:i_NumObs,1:i_NumVars), STAT=ios)
IF (ios /= i_OK) THEN
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |________________
!  |                                                   |
!  | 'Memory allocation error: da_S,da_X in EMRSPDVAL' |
!  |___________________________________________________|
!
   i_CCode = ios
   iEr = i_MRSPMOD_ER007
   RETURN   ! ... error exit ...
END IF
DO i=1,i_NumObs
   da_X(i,1:i_NumVars) = da_Datas(ia_P(i),1:i_NumVars)
END DO
d_Delta = d_ZERO
DO i=1,i_NumObsM1
   d_Sum = d_ZERO
   DO k=1,i_NumVars
      d_Sum = d_Sum + (da_X(i,k)-da_X(i+1,k))**2
   END DO
   !.. d_Sum   = SUM((da_X(I,1:i_NumVars)-da_X(I+1,1:i_NumVars))**2)
   d_Delta = d_Delta + d_Sum**(d_DistExpon/d_TWO)
END DO
d_Delta = d_Delta/d_NumObsM1
IF (d_Delta < d_Delta1) i_Kount2 = i_Kount2 + 1
DEALLOCATE(da_X, STAT=ios)
RETURN   ! ... normal exit ...
END SUBROUTINE emrspdval

!=============================================================================!

END MODULE mrspmod


MODULE domrspmod
IMPLICIT NONE
!  Modified this file to be a module - JDR 7/15/2002
CONTAINS
!  DOMRSP   <File: "domrsp.f90: domrsp">
!  SPOPTION <File: "domrsp.f90: spoption">
!  SPDOSET  <File: "domrsp.f90: spdoset">
!  RUNMRSP  <File: "domrsp.f90: runmrsp">
!  DISPMRSP <File: "domrsp.f90: dispmrsp">


SUBROUTINE runmrsp(da_Data,d_V_DistExp,i_NumObs,i_NumVars,i_NumPerms,l_DoEMRSP,l_DoResample,l_IsCommens,&
				 d_TestStat,     &
                 d_ObsDelta,     &
                 d_ExpDelta,     &
                 d_VarDelta,     &
                 d_SkwDelta,     &
                 d_RhoAgreement, &
                 d_PValue,       &
                 iEr,			 &
                 l_SaveTest,da_STV,i_seed,da_AvgCommDist)

!___Imported Parameters and Variables:
USE ablparms, ONLY: i_IN_BUFFER_SIZE
        ! ^-- High-level Blossom parametes.

USE blcmnmod, ONLY: cha_CmdStack, cha_VarList, ch_NL, ch_Output_Title,        &
      ia_ActiveVarNdx, i_NOT_OK, i_OK, i_ZERO,         &
      l_CLEAR, l_Terse
        ! ^-- Some global Blossom variables and parameters.
USE ioformod, ONLY: ch12_OutBuff, ch12_OutBuff2
        ! ^-- Output buffer for character output of INTEGER number.
USE jonsmodule, ONLY: ch_EM00033, ch_EM00047, ch_EM00052, ch_EM00053,         &
      ch_EM00185, ch_EM00186, ch_EM00200, ch_MRSPMOD_EM002, ch_MRSPMOD_EM003, &
      ch_MRSPMOD_EM004, ch_MRSPMOD_EM006, ch_MRSPMOD_EM007, ch_STR0004,       &
      ch_STR0006, ch_STR0011, ch_STR0013, ch_STR0031, i_CCode,                &
      i_MRSPMOD_ER002, i_MRSPMOD_ER003, i_MRSPMOD_ER004, i_MRSPMOD_ER006,     &
      i_MRSPMOD_ER007
        ! ^-- Message handling, etc.
USE missvmod, ONLY: d_MISSING_VALUE
        ! ^-- values to use for missing values representation.
USE mrolamod, ONLY: cha_MRVarList, ch_SPVarName, i_HUGE_EMRSP_DATA,           &
      i_LARGE_EMRSP_DATA, i_MIN_EMRSP_OBS, i_MIN_MRSP_OBS,                    &
      i_NumRcd,                       &
       i_USE_SYS_RAND,       &
      l_DoScreen4MsngVals, l_HasSPVar
        ! ^-- Permutation, regression and other Blossom stat parameters.
!___Imported Procedures:
USE mrppmod, ONLY: commenspp
        ! ^-- commenspp: Commensurate multivariate variable data.
USE jonsmodule, ONLY: errhand, getrseed, lf_Equals, lf_IsIn
        ! ^-- emitstr: Display a string to the console output stream.
        ! ^-- errhand: Display conditions causing error problem.
        ! ^-- getrseed: get random number seed.
        ! ^-- lf_Equals: Generic test for equality of two objects.
        ! ^-- lf_IsIn(string,substring): Return TRUE if substring is in string.

USE mrspmod, ONLY: emrsp, mrsp3
        ! ^-- mrsp: Perform a Mulit-Response Sequence Procedure.
        ! ^-- emrsp: Perform an Exact Mulit-Response Sequence Procedure.
USE mt19937, ONLY: init_genrand
        ! ^-- Mersenne Twister Pseudo Random Number Generator
IMPLICIT NONE
!___Dummy Arguments:
REAL (KIND(0D0)), INTENT(INOUT):: da_Data(:,:)
REAL (KIND(0D0)), INTENT(IN)  :: d_V_DistExp
INTEGER,          INTENT(IN)   :: i_NumObs
INTEGER,          INTENT(IN)   :: i_NumVars
INTEGER,          INTENT(IN)  ::i_NumPerms
LOGICAL,          INTENT(IN)  ::l_DoEMRSP
LOGICAL,          INTENT(IN)  ::l_DoResample
LOGICAL,          INTENT(IN)  ::l_IsCommens
REAL (KIND(0D0)), INTENT(OUT)  :: d_TestStat
REAL (KIND(0D0)), INTENT(OUT)  :: d_ObsDelta
REAL (KIND(0D0)), INTENT(OUT)  :: d_ExpDelta
REAL (KIND(0D0)), INTENT(OUT)  :: d_VarDelta
REAL (KIND(0D0)), INTENT(OUT)  :: d_SkwDelta
REAL (KIND(0D0)), INTENT(OUT)  :: d_RhoAgreement
REAL (KIND(0D0)), INTENT(OUT)  :: d_PValue
INTEGER,          INTENT(OUT) :: iEr
REAL (KIND(0D0)), INTENT(OUT) :: da_STV(:)  ! save test values
INTEGER,          INTENT(INOUT)    :: i_seed
LOGICAL,  		   INTENT (IN)   :: l_SaveTest
REAL (KIND(0D0)), INTENT(OUT) :: da_AvgCommDist(:)
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     SUBROUTINE runmrsp
! DESCRIPTION:
!     Run Multi-Response Sequence Procedure.
! LANGUAGE:
!     Fortran 90/95
! AUTHOR:
!     Jon D. Richards, Operations Research Analyst       jon_richards@usgs.gov
!     Fort Collins Science Center                     http://www.fort.usgs.gov
!     U.S. Geological Survey          -          Biological Resources Division
! INVOKED BY:
!     spdoset         <File: "domrsp.f90: spdoset">           module
! INVOKES:
!     commenspp       <File: "cmsramod.f90: commenspp">       module
!     dispmrsp        <File: "domrsp.f90: dispmrsp">          module
!     emrsp           <File: "mrspmod.f90: emrsp">            module
!     emitstr         <File: "jonsmodule.f90: emitstr">       module
!     getactivevarlst <File: "mrauxmod.f90: getactivevarlst"> module
!     getrseed        <File: "jonsmodule.f90: getrseed">      module
!     lf_Equals       <File: "jonsmodule.f90: lf_Equals">     module
!     lf_IsIn         <File: "jonsmodule.f90: lf_IsIn">       module
!     mrsp3           <File: "mrspmod.f90: mrsp3">            module
!     Winteracter:
!           WInfoDialog
!           WMessageBox

! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!___Procedure Parameters:
CHARACTER (LEN=*), PARAMETER :: ch_PROC_NAME = "RUNMRSP"
!___Local Variables:

INTEGER :: i, j, k, l
INTEGER :: ios
INTEGER :: i_Exc
INTEGER :: i_IC, i_Factorial
LOGICAL :: l_HasMissingValueThisCase
CHARACTER (LEN=i_IN_BUFFER_SIZE) :: ch_InputBuff
CHARACTER (LEN=4) :: blcmd
INTEGER, PARAMETER :: i_LEN_MY_EMSG = 254 ! needs to be this big for output of var list in dostatus.
CHARACTER (LEN=i_LEN_MY_EMSG) :: ch_MyMsg
INTEGER :: i_numcmdgpvarvals
INTEGER :: i_numholdcases
REAL (KIND(0D0)), ALLOCATABLE :: da_Values(:)
INTEGER :: i_NumCases
!___Intrinsic Procedures:
INTRINSIC :: ALLOCATED, RANDOM_SEED, SIZE
!__Executable Statements:

iEr = i_OK ! no problem yet
! Get the data, either in case order or sorted by the
! sequence variable order. Use only variables, and in the
! order, of those on the MRSP command line.

! Check some limits before we go further.
i_NumCases=i_NumObs


! If Exact MRSP, then warn user of N! computation times if N (number
! of observations) is greater than i_LARGE_EMRSP_DATA.


ALLOCATE(                                           &
           da_Values(1:i_NumVars),                  &
      STAT=ios)
IF (ios /= i_OK) THEN
   CALL errhand(ch_MyMsg,i_Code=ios,        &
                ch_Msg=ch_EM00033, &
                 ch_S1="da_Values,da_Data,da_AvgCommDist", &
                 ch_S2=ch_PROC_NAME)
   DEALLOCATE(da_Values, STAT=ios)
   iEr = i_NOT_OK
   GOTO 7000
END IF

! If we need to commensurate the (multivariate) data, do so:
IF (l_IsCommens) THEN
   CALL commenspp(i_NumCases,     & !* <File: "cmsramod.f90: commenspp">
                  i_NumVars,    &
                  d_V_DistExp,    &
                  da_Data,        &
                  da_AvgCommDist, &
                  iEr)
   IF (iEr /= i_OK) THEN
      iEr = i_NOT_OK
      GOTO 7000
   END IF
END IF

! Do the Sequence Procedures:
IF (l_DoEMRSP) THEN  ! EXACT MRSP:

   i_IC = i_ZERO
   CALL emrsp(i_IC,        & !* <File: "mrspmod.f90: emrsp">
              i_NumCases,  &
              i_NumVars, &
              d_V_DistExp, &
              da_Data,     &
              d_ObsDelta,  &
              d_PValue,    &
              i_Factorial, &
              iEr)
ELSE                ! MRSP:

   IF (i_Seed == i_USE_SYS_RAND) THEN
      CALL getrseed(i_Seed) !* <File: "jonsmodule.f90: getrseed">
      ! Initialize default PRNG
      CALL RANDOM_SEED(SIZE=k)
      CALL RANDOM_SEED(PUT=(/(i_Seed,i=1,k)/))
      ! Initialize Mersenne Twister PRNG
      CALL init_genrand(i_Seed)
   END IF

   ! we may want to allow for 2d distance matrices again, so the following
   ! nonsense call to mrsp32d is coded here. For now, we always to to the
   ! call mrsp3, which uses the 1d distance matrix. - jdr

      CALL mrsp3(i_NumCases, i_NumVars, i_NumPerms, l_DoResample, &
            d_V_DistExp, da_Data, d_TestStat, d_ObsDelta, d_ExpDelta, &
            d_VarDelta, d_SkwDelta, d_RhoAgreement, d_PValue, iEr,l_SaveTest,da_STV,i_seed) !* <File: "mrspmod.f90: mrsp3">

END IF
IF (iEr /= i_OK) THEN      ! BEGIN Error Handling
   SELECT CASE (iEr)  ! Error handling
      CASE (i_MRSPMOD_ER002)
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |____________
!  |                                               |
!  | 'Memory allocation error: da_Del in MRSPDIST' |
!  |_______________________________________________|
!
         CALL errhand(ch_MyMsg,i_Code=i_CCode, &
                      ch_Msg=ch_MRSPMOD_EM002)
      CASE (i_MRSPMOD_ER003)
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |___________
!  |                                              |
!  | 'Memory allocation error: da_DC in MRSPDIST' |
!  |______________________________________________|
!
         CALL errhand(ch_MyMsg,i_Code=i_CCode, &
                      ch_Msg=ch_MRSPMOD_EM003)
      CASE (i_MRSPMOD_ER004)
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |_____________
!  |                                                |
!  | 'Memory allocation error: da_AD,da_S in EMRSP' |
!  |________________________________________________|
!
         CALL errhand(ch_MyMsg,i_Code=i_CCode, &
                      ch_Msg=ch_MRSPMOD_EM004)
      CASE (i_MRSPMOD_ER006)
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |_________________
!  |                                                    |
!  | 'Memory allocation error: ia_P, ia_A in EMRSPFACT' |
!  |____________________________________________________|
!
         CALL errhand(ch_MyMsg,i_Code=i_CCode, &
                      ch_Msg=ch_MRSPMOD_EM006)
      CASE (i_MRSPMOD_ER007)
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |________________
!  |                                                   |
!  | 'Memory allocation error: da_S,da_X in EMRSPDVAL' |
!  |___________________________________________________|
!
         CALL errhand(ch_MyMsg,i_Code=i_CCode, &
                      ch_Msg=ch_MRSPMOD_EM007)
      CASE DEFAULT
   END SELECT
   iEr = i_NOT_OK
   GOTO 7000
END IF                     ! END Error Handling




7000 CONTINUE
IF (iEr /= i_OK) THEN  ! error exit
   i_NumHoldCases    = 0
   i_NumCmdGpVarVals = 0
END IF
 IF (ALLOCATED(da_Values))      DEALLOCATE(da_Values,      STAT=ios)
 
 IF (iEr /= i_OK) THEN
   call rexit(ch_MyMsg)
END IF

RETURN   ! ... normal exit ...
END SUBROUTINE runmrsp

!=============================================================================!


END MODULE domrspmod

!=============================================================================!
MODULE regrsmod
!d USE debugmod                                                        ! debug!
! USE binomod, ONLY: random_binomial ! btpec90
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     MODULE regrsmod
! DESCRIPTION:
!     Subroutines for the Least Absolute Deviation (Median) and
!     Quantile regressions as well as the Ordinary Least Squares
!     Regression and the testing for those models.
!   Revision - Jan 2004
! LAD12 and called routines:
! lqleast, ftlqleast, htlqleast, htqm1least, htrsleast, htrsdpleast, htdpleast
!   This simplifies incredibly complicated code in the older
!   LAD subroutine and concommitant "least"-style routines.  The older version
!   was developed incrmentally, with revisions and updates inserted into
!   existing code.  Code exectution paths were tortuous through flow control
!   structures that were nearly impossible to decypher after 10 or 12 major
!   changes were made (several were abandonned, and some residual code for
!   those still existed in the older code).
!   The new code takes a fresh approach that identifies 12 separate paths
!   that regression analysis can take.  Eight (8) of these pertain to lad/
!   quantile regression.  The LAD12 routine now identifies these paths and
!   calls separate "least"-like routines that perform the regressions and
!   the tests.  In several places a layer or two of procedure calls was
!   eliminated.  Extra array allocations were eliminated.  The newer
!   code more explicitly states the logic of the method.  It should be
!   significantly faster than the old code.  Some problems were identified
!   in the old code that were fixed in order to test the new code properly.
! LANGUAGE:
!     Fortran 90/95
! AUTHOR:
!     Jon D. Richards, Operations Research Analyst       jon_richards@usgs.gov
!     Fort Collins Science Center                     http://www.fort.usgs.gov
!     U.S. Geological Survey          -          Biological Resources Division
!     Original statistical programs from Dr. Paul Mielke, Colo. State Univ.
!        http://www.stat.colostate.edu/~mielke/mielkesoftware.html
! MODIFICATION HISTORY:
!     Module created - JDR  26 Oct 1999
!     Mod - Testing and debugging - minor adjustments for carrying values back
!           to calling routines instead of writing to files. - JDR 19 Apr 2000
!     Mod - Implemented double permutation testing of lad and quantile
!           regressions (with no intercept). Took out internal permut routines
!           and made common routine. Since double permute operates from LAD
!           routine, moved allocation and deallocation of temporary arrays
!           from LEAST and RSOLSLEAST to LAD.  Added littlemedq1 function to
!           get a value for an array at a given quantile.  Added chngxon
!           routine to change a given number of old values to new values in
!           an array.  Links to random_binonmial (which uses method of Kemp)
!           after experimenting with several alternatives. Moved saving
!           regression results to new LADSAVE and LSQSAVE routines. Created
!           new debugging module and added calls to this--really should
!           leave this out of the production code.  - JDR 1 Aug 2003
!     Mod - added: - JDR 15 Jan 2004
!           LAD12 and called routines:
!           lqleast, ftlqleast, htlqleast, htqm1least, htrsleast,
!           htrsdpleast, htdpleast. see note in DESCRIPTION, above.
!     Mod - Added option to have SAVETEST of regression test statistics for
!           original and permuted regressions to a file - JDR  24 Mar 2005
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
IMPLICIT NONE
SAVE
!___Module Parameters:
REAL (KIND(0D0)), PARAMETER :: d_LARGE_VALUE = 1.0D300
REAL (KIND(0D0)), PARAMETER :: d_SMALL_VALUE = 1.0D-015
!d LOGICAL :: l_ON                                                     ! debug!
CONTAINS
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     LAD12           <File: "regrsmod.f90: lad12">
!        LQLEAST      <File: "regrsmod.f90: lqleast">
!        FTLQLEAST    <File: "regrsmod.f90: ftlqleast">
!        HTLQLEAST    <File: "regrsmod.f90: htlqleast">
!        HTQM1LEAST   <File: "regrsmod.f90: htqm1least">
!        HTRSLEAST    <File: "regrsmod.f90: htrsleast">
!        HTRSDPLEAST  <File: "regrsmod.f90: htrsdpleast">
!        HTDPLEAST    <File: "regrsmod.f90: htdpleast">
!     IPERMUT         <File: "regrsmod.f90: permut">
!     PERMUT          <File: "regrsmod.f90: permut">
!     **(OUT) LEAST <File: "regrsmod.f90: least">**
!     LADSAVE         <File: "regrsmod.f90: ladsave">
!     L1              <File: "regrsmod.f90: L1">
!     **(OUT) LSQ <File: "regrsmod.f90: lsq">**
!     LSQLEAST        <File: "regrsmod.f90: lsqleast">
!     LSQSAVE         <File: "regrsmod.f90: lsqsave">
!     RQBR            <File: "regrsmod.f90: rqbr">
!     CHNGXON         <File: "regrsmod.f90: chngxon">
!     LITTLEMEDQ1     <File: "regrsmod.f90: littlemedq1">
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !

!==============================================================================

SUBROUTINE lad12(i_NumObs, i_NumVars, d_Theta, i_NumPermut, i_RandNumSeed, &
                 i_NumToDrop, ia_PosToDrop, d_Toler, l_SaveTest, i_Test, &
                 l_DoublePermutation, &
                 da_Data, &
                 d_To, d_Tn, i_CntSumAVR, d_PValue, d_PVTn, &
                 da_Betas, da_RedBetas, &
                 d_SumAbsValRes, d_SumAbsValResRed, &
                 d_WtSumAbsDevsFulMod, d_WtSumAbsDevsRedMod, &
                 i_Iter, i_ExitCode, iEr,ch_MyMsg,da_STV,i_LaHy,da_ResRed,da_Resids)
!___Imported Parameters and Variables:
USE comndmod, ONLY: i_RegressionMode, UNASSIGNED, SIMPLE_LAD, SIMPLE_QUANT, &
      LAD_TEST, HYP_LAD_QUANT, HYP_DELETE_QM1, HYP_RANKSCORE, &
      HYP_RANKSCORE_DP, HYP_DOUBLE_PERM, HYP_QM1_AND_DP
        ! ^-- Command type codes and command abbreviations.
USE blcmnmod, ONLY: d_ONE, d_ZERO, i_OK, i_NOT_OK
        ! ^-- Some global Blossom variables and parameters.
USE jonsmodule, ONLY: ch_EM00033, ch_REGRSMOD_EM019
        ! ^-- Message handling, etc.
USE missvmod, ONLY: d_MISSING_VALUE
        ! ^-- Missing values
USE mrolamod, ONLY: i_DO_FULL_TEST, i_DO_RANK_SCORE_TEST, &
      i_PROG_TYPE_HYP
        ! ^-- Permutation, regression and other Blossom stat parameters.
!___Imported Procedures:
USE jonsmodule, ONLY: errhand
       
IMPLICIT NONE
!___Dummy Arguments:
INTEGER,          INTENT(IN)    :: i_NumObs
INTEGER,          INTENT(IN)    :: i_NumVars
REAL (KIND(0D0)), INTENT(IN)    :: d_Theta
INTEGER,          INTENT(IN)    :: i_NumPermut
INTEGER,          INTENT(IN)    :: i_RandNumSeed
INTEGER,          INTENT(IN)    :: i_NumToDrop
INTEGER,          INTENT(IN)    :: ia_PosToDrop(:)
REAL (KIND(0D0)), INTENT(IN)    :: d_Toler
LOGICAL,          INTENT(IN)    :: l_SaveTest
LOGICAL,          INTENT(IN)    :: l_DoublePermutation
INTEGER,          INTENT(IN)    :: i_Test
REAL (KIND(0D0)), INTENT(INOUT) :: da_Data(:,:)
REAL (KIND(0D0)), INTENT(OUT)   :: d_To
REAL (KIND(0D0)), INTENT(OUT)   :: d_Tn
INTEGER,          INTENT(OUT)   :: i_CntSumAVR
REAL (KIND(0D0)), INTENT(OUT)   :: d_PValue
REAL (KIND(0D0)), INTENT(OUT)   :: d_PVTn
REAL (KIND(0D0)), INTENT(OUT)   :: da_Betas(:)
REAL (KIND(0D0)), INTENT(OUT)   :: da_RedBetas(:)
REAL (KIND(0D0)), INTENT(OUT)   :: d_SumAbsValRes
REAL (KIND(0D0)), INTENT(OUT)   :: d_SumAbsValResRed
REAL (KIND(0D0)), INTENT(OUT)   :: d_WtSumAbsDevsFulMod
REAL (KIND(0D0)), INTENT(OUT)   :: d_WtSumAbsDevsRedMod
INTEGER,          INTENT(OUT)   :: i_Iter
INTEGER,          INTENT(OUT)   :: i_ExitCode
INTEGER,          INTENT(OUT)   :: iEr
INTEGER, PARAMETER :: i_LEN_MY_EMSG = 254 ! needs to be this big for output of var list in dostatus.
CHARACTER (LEN=i_LEN_MY_EMSG), INTENT(OUT) :: ch_MyMsg
REAL (KIND(0D0)), INTENT(OUT) :: da_STV(:)  ! save test values
INTEGER, 		 INTENT(INOUT) ::i_LaHy
REAL (KIND(0D0)), INTENT(OUT)      :: da_ResRed(:)     ! (1:i_NumObs)
REAL (KIND(0D0)), INTENT(OUT)      :: da_Resids(:)     ! (1:i_NumObs)
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
!   SUBROUTINE LAD12
! DESCRIPTION:
!     Ascertain the path to follow based on the commands given by user. Then
! LANGUAGE:
!     Fortran 90/95
! AUTHOR:
!     Jon D. Richards, Operations Research Analyst    jon_richards@usgs.gov
!     Fort Collins Science Center                  http://www.fort.usgs.gov
!     U.S. Geological Survey  -  Biological Resources Division
! MODIFICATION HISTORY:
!   CODE HISTORY:
!     CREATED - JDR Jan 2004
!     MOD 4 April 2005 - Add the QM1 and Double Permutation combination
!               hypothesis testing - JDR Apr 2005
!   COMMENT HISTORY:
!     CREATED - JDR Jan 2004!
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
!___Local Variables:


INTEGER :: i_NumVRed, ios
LOGICAL :: l_DoQM1, l_DoRankScore, l_IsATest, l_ReducedModel, &
      l_TestFullModel, l_TrueLad, l_DoQM1DP
!___Intrinsic Procedures:
INTRINSIC :: ALLOCATED, HUGE
!___Executable Statements:

iEr = i_OK ! no problem yet
d_PVTn = d_MISSING_VALUE  ! We won't always compute this
d_Tn = d_MISSING_VALUE  ! We won't always compute this
da_RedBetas = d_ZERO
d_SumAbsValRes = d_ZERO
d_SumAbsValResRed = d_ZERO
d_WtSumAbsDevsFulMod = d_ZERO
d_WtSumAbsDevsRedMod = d_ZERO
i_CntSumAVR = 0
l_DoQM1DP = .FALSE.


IF (d_Theta < d_ZERO) THEN   ! ---- July 13 1998 code added
   l_TrueLad = .TRUE.        ! Quant Reg indicated by d_Theta
ELSE                         ! non-negative in our coding.
   l_TrueLad = .FALSE.       ! A negative theta indicates
END IF                       ! a true "LAD" regression .
IF (i_Test == i_DO_FULL_TEST) THEN        ! is this full model to test?

   l_TestFullModel = .TRUE.
   l_DoRankScore   = .FALSE.
ELSE IF (i_Test == i_DO_RANK_SCORE_TEST) THEN  ! Means do Rank Scores.
   l_TestFullModel = .FALSE.
   l_DoRankScore   = .TRUE.
ELSE
   l_TestFullModel = .FALSE.
   l_DoRankScore   = .FALSE.
END IF
IF (i_NumToDrop > 0) THEN  ! how we find if this will be a reduced model:
   l_ReducedModel = .TRUE. ! one or more vars dropped from full model
   i_NumVRed = i_NumVars - i_NumToDrop
ELSE
   l_ReducedModel = .FALSE.
END IF
IF (l_TestFullModel  .OR.  &    ! Find out if there is testing
    l_ReducedModel  .OR.  &
    l_DoRankScore) THEN  ! if test:
   l_IsATest = .TRUE.

ELSE                    ! if not test:
   l_IsATest = .FALSE.
END IF
IF (i_LaHy == i_PROG_TYPE_HYP  .AND.  &
    l_ReducedModel  .AND.  &
    i_NumVRed > 1  .AND.  &
    .NOT.l_DoRankScore) THEN ! Find out if this is a drop qm1 zeroes test
   l_DoQM1 = .TRUE.
   !

   IF (l_DoublePermutation) THEN ! Set up to do both QM1 and DP
      l_DoQM1 = .FALSE. ! Not if also a double permutation
      ! l_DoublePermutation = .FALSE. ! Also don't do a DP only, ...
      l_DoQM1DP = .TRUE. ! Do combination of both QM1 and DP
   END IF
   !

   !
ELSE
   l_DoQM1 = .FALSE.
END IF

i_RegressionMode = UNASSIGNED
IF (l_IsATest) THEN
   IF (l_DoQM1DP) THEN
      i_RegressionMode = HYP_QM1_AND_DP

   ELSE IF (l_DoQM1) THEN
      i_RegressionMode = HYP_DELETE_QM1

   ELSE IF (l_TestFullModel) THEN
      i_RegressionMode = LAD_TEST

   ELSE IF (l_DoublePermutation .AND.  &
       .NOT.l_DoRankScore  .AND.  &
       i_LaHy == i_PROG_TYPE_HYP) THEN
      i_RegressionMode = HYP_DOUBLE_PERM

   ELSE IF (l_DoRankScore  .AND.  &
       i_LaHy == i_PROG_TYPE_HYP) THEN
      IF (l_DoublePermutation) THEN
         i_RegressionMode = HYP_RANKSCORE_DP

      ELSE
         i_RegressionMode = HYP_RANKSCORE

      END IF
   ELSE
      i_RegressionMode = HYP_LAD_QUANT

   END IF
ELSE ! .NOT.l_IsATest
   IF (l_TrueLad) THEN
      i_RegressionMode = SIMPLE_LAD

   ELSE
      i_RegressionMode = SIMPLE_QUANT
                              ! debug!
   END IF
END IF



SELECT CASE (i_RegressionMode)
   CASE (SIMPLE_LAD)

      CALL lqleast(i_NumObs, i_NumVars, d_Theta, d_Toler, l_SaveTest, &
                   da_Data, &
                   da_Betas, da_Resids, &
                   d_SumAbsValRes, d_WtSumAbsDevsFulMod, &
                   i_Iter, i_ExitCode, iEr,ch_MyMsg)
      d_PValue = -1.0D0 ! don't really have a p-value
   CASE (SIMPLE_QUANT)

      CALL lqleast(i_NumObs, i_NumVars, d_Theta, d_Toler, l_SaveTest, &
                   da_Data, &
                   da_Betas, da_Resids, &
                   d_SumAbsValRes, d_WtSumAbsDevsFulMod, &
                   i_Iter, i_ExitCode, iEr,ch_MyMsg)

      d_PValue = -1.0D0 ! don't really have a p-value
   ! Testing:
   !   Full Test of LAD/QUANT:
   CASE (LAD_TEST)

      CALL ftlqleast(i_NumObs, i_NumVars, d_Theta, i_NumPermut, &
                     i_RandNumSeed, d_Toler, l_SaveTest, &
                     da_Data, &
                     i_CntSumAVR, d_PValue, &
                     da_Betas, da_Resids, &
                     d_SumAbsValRes, d_WtSumAbsDevsFulMod, &
                     i_Iter, i_ExitCode, iEr,ch_MyMsg)
   !   Hypothesis Testing of LAD/QUANT:
   CASE (HYP_LAD_QUANT)

      CALL htlqleast(i_NumObs, i_NumVars, d_Theta, i_NumPermut, &
                     i_RandNumSeed, i_NumToDrop, ia_PosToDrop, d_Toler, &
                     da_Data, &
                     d_To, d_PValue, &
                     da_Betas, da_RedBetas, &
                     da_Resids, da_ResRed, &
                     d_SumAbsValRes, d_SumAbsValResRed, &
                     d_WtSumAbsDevsFulMod, d_WtSumAbsDevsRedMod, &
                     i_Iter, i_ExitCode, iEr,ch_MyMsg,da_STV,l_SaveTest)
   CASE (HYP_DELETE_QM1)

      CALL htqm1least(i_NumObs, i_NumVars, d_Theta, i_NumPermut, &
                      i_RandNumSeed, i_NumToDrop, ia_PosToDrop, d_Toler, &
                      da_Data, &
                      d_To, d_PValue, &
                      da_Betas, da_RedBetas, &
                      da_Resids, da_ResRed, &
                      d_SumAbsValResRed, &
                      d_WtSumAbsDevsRedMod, &
                      i_Iter, i_ExitCode, iEr,ch_MyMsg,da_STV,l_SaveTest)
   CASE (HYP_RANKSCORE)

      CALL htrsleast(i_NumObs, i_NumVars, d_Theta, i_NumPermut, &
                     i_RandNumSeed, i_NumToDrop, ia_PosToDrop, d_Toler, &
                     da_Data, &
                     d_To, d_Tn, d_PValue, d_PVTn, &
                     da_Betas, da_RedBetas, &
                     da_Resids, da_ResRed, &
                     d_SumAbsValResRed, &
                     d_WtSumAbsDevsRedMod, &
                     i_Iter, i_ExitCode, iEr,ch_MyMsg,da_STV,l_SaveTest)
   CASE (HYP_RANKSCORE_DP)

      CALL htrsdpleast(i_NumObs, i_NumVars, d_Theta, i_NumPermut, &
                       i_RandNumSeed, i_NumToDrop, ia_PosToDrop, d_Toler, &
                       da_Data, &
                       d_To, d_Tn, d_PValue, d_PVTn, &
                       da_Betas, da_RedBetas, &
                       da_Resids, da_ResRed, &
                       d_SumAbsValResRed, &
                       d_WtSumAbsDevsRedMod, &
                       i_Iter, i_ExitCode, iEr,ch_MyMsg,da_STV,l_SaveTest)
   CASE (HYP_DOUBLE_PERM)

      CALL htdpleast(i_NumObs, i_NumVars, d_Theta, i_NumPermut, &
                     i_RandNumSeed, i_NumToDrop, ia_PosToDrop, d_Toler, &
                     da_Data, &
                     d_To, d_PValue, &
                     da_Betas, da_RedBetas, &
                     da_Resids, da_ResRed, &
                     d_SumAbsValResRed, &
                     d_WtSumAbsDevsRedMod, &
                     i_Iter, i_ExitCode, iEr,ch_MyMsg,da_STV,l_SaveTest)
   CASE (HYP_QM1_AND_DP)

      CALL htqm1dpleast(i_NumObs, i_NumVars, d_Theta, i_NumPermut, &
                        i_RandNumSeed, i_NumToDrop, ia_PosToDrop, d_Toler, &
                        da_Data, &
                        d_To, d_PValue, &
                        da_Betas, da_RedBetas, &
                        da_Resids, da_ResRed, &
                        d_SumAbsValResRed, &
                        d_WtSumAbsDevsRedMod, &
                        i_Iter, i_ExitCode, iEr,ch_MyMsg,da_STV,l_SaveTest)
   CASE DEFAULT

!////////////////////////////////////////////////////////////////////////////
!  ____________________________________________________
! |                                                    |
! | Regression mode unassigned in LAD12, program error |
! |____________________________________________________|
!
      CALL errhand(ch_MyMsg,ch_Msg=ch_REGRSMOD_EM019)
!////////////////////////////////////////////////////////////////////////////
      iEr = UNASSIGNED
      RETURN
END SELECT

80000 CONTINUE


RETURN
END SUBROUTINE lad12

!==============================================================================

SUBROUTINE lqleast(i_NumObs, i_NumVars, d_Theta, d_Toler, l_DoLadSave, &
                   da_Data, &
                   da_Betas, da_Resids, &
                   d_SumAbsValRes, d_WtSumAbsDevsFulMod, &
                   i_Iter, i_ExitCode, iEr,ch_MyMsg)
!___Imported Parameters and Variables:
USE blcmnmod, ONLY: d_ONE, d_ZERO, i_OK, i_NOT_OK
        ! ^-- Some global Blossom variables and parameters.
USE jonsmodule, ONLY: i_CCode, i_REGSRMOD_ER001, i_REGSRMOD_ER004, &
      i_REGSRMOD_ER011, ch_EM00033
        ! ^-- Message handling, etc.
!___Imported Procedures:
USE jonsmodule, ONLY: errhand
      
IMPLICIT NONE
!___Dummy Arguments:
INTEGER,          INTENT(IN)    :: i_NumObs
INTEGER,          INTENT(IN)    :: i_NumVars
REAL (KIND(0D0)), INTENT(IN)    :: d_Theta
REAL (KIND(0D0)), INTENT(IN)    :: d_Toler
LOGICAL,          INTENT(IN)    :: l_DoLadSave
REAL (KIND(0D0)), INTENT(INOUT) :: da_Data(:,:)
REAL (KIND(0D0)), INTENT(OUT)   :: da_Betas(:)
REAL (KIND(0D0)), INTENT(OUT)   :: da_Resids(:)
REAL (KIND(0D0)), INTENT(OUT)   :: d_SumAbsValRes
REAL (KIND(0D0)), INTENT(OUT)   :: d_WtSumAbsDevsFulMod
INTEGER,          INTENT(OUT)   :: i_Iter
INTEGER,          INTENT(OUT)   :: i_ExitCode
INTEGER,          INTENT(OUT)   :: iEr
INTEGER, PARAMETER :: i_LEN_MY_EMSG = 254 ! needs to be this big for output of var list in dostatus.
CHARACTER (LEN=i_LEN_MY_EMSG), INTENT(OUT) :: ch_MyMsg

! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
!     SUBROUTINE LQLEAST
! DESCRIPTION:
!     Perform a lad or quantile regression.
!
! LAD ........................................................(SIMPLE_LAD)
!    Condition:  Command is LAD  (This is a "True" LAD regression)
!
!       Action:  -> L1 ->
!
!       Output: Betas,
!               Sum of absolute values of the residuals
!
!
! LAD / QUANT=t ............................................(SIMPLE_QUANT)
!    Condition:  Command is LAD / QUANT=t (This is a quantile regression)
!
!       Action:  -> L1 ->
!
!       OutPut: Betas,
!               Sum of absolute values of the residuals,
!               Weighted sum of the absolute deviations
!
! LANGUAGE:
!     Fortran 90/95
! AUTHOR:
!     Jon D. Richards, Operations Research Analyst    jon_richards@usgs.gov
!     Fort Collins Science Center                  http://www.fort.usgs.gov
!     U.S. Geological Survey  -  Biological Resources Division
! MODIFICATION HISTORY:
!   CODE HISTORY:
!     CREATED - JDR Jan 2004
!   COMMENT HISTORY:
!     CREATED - JDR Jan 2004!
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
!___Local Variables:
REAL (KIND(0D0)), ALLOCATABLE :: da_DataSave(:,:)
REAL (KIND(0D0)), ALLOCATABLE :: da_DepVarVals(:)
REAL (KIND(0D0)), ALLOCATABLE :: da_Y(:) ! copy dep var vals to SAVE
REAL (KIND(0D0)) :: d_TempTheta
INTEGER, ALLOCATABLE :: ia_Work(:)
INTEGER :: i, i_NumObsP1, i_NumObsP2, i_NumVarsP1, i_NumVarsP2, ios
LOGICAL :: l_TrueLad
!___Intrinsic Procedures:
INTRINSIC :: ALLOCATED, HUGE, INT
!___Executable Statements:

iEr = i_OK ! no problem yet
i_Iter = 0
i_ExitCode = -HUGE(1) ! absurd value
d_SumAbsValRes       = -HUGE(d_ONE) ! absurd value
d_WtSumAbsDevsFulMod = -HUGE(d_ONE) ! absurd value
IF (d_Theta < d_ZERO) THEN
   l_TrueLad = .TRUE.
   d_TempTheta = -d_Theta
ELSE
   d_TempTheta = d_Theta
   l_TrueLad = .FALSE.
END IF
i_NumObsP1  = i_NumObs  + 1
i_NumVarsP1 = i_NumVars + 1
i_NumObsP2  = i_NumObs  + 2
i_NumVarsP2 = i_NumVars + 2
ALLOCATE(ia_Work(1:i_NumObs), &
   da_DepVarVals(1:i_NumObs), &
            STAT=ios)
IF (ios /= i_OK) THEN ! Memory allocation error
   CALL errhand(ch_MyMsg,i_Code=ios, &
                ch_Msg=ch_EM00033, &
                ch_S1="ia_Work,da_DepVarVals", &
                ch_S2="lqleast")
   iEr = i_NOT_OK
   GOTO 80000
END IF
IF (l_DoLadSave) THEN ! only need copy if we do a LAD save
   ALLOCATE(da_DataSave(1:i_NumObsP1,1:i_NumVarsP1), &
                   da_Y(1:i_NumObs), &
                     STAT=ios)
   IF (ios /= i_OK) THEN ! Memory allocation error: da_DataSave in LEAST
      CALL errhand(ch_MyMsg,i_Code=ios, &
                   ch_Msg=ch_EM00033, &
                   ch_S1="da_DataSave,da_Y", &
                   ch_S2="lqleast")
      iEr = i_NOT_OK
      GOTO 80000
   END IF
   da_DataSave(1:i_NumObs,1:i_NumVarsP1) = da_Data(1:i_NumObs,1:i_NumVarsP1)
END IF
da_DepVarVals(1:i_NumObs) = da_Data(1:i_NumObs,i_NumVarsP1) ! copy to send L1
IF (l_DoLadSave) da_Y(1:i_NumObs) = da_DepVarVals(1:i_NumObs) ! keep to send to SAVE
CALL L1(i_NumObs, i_NumVars, i_NumObsP2, i_NumVarsP2, da_Data, da_DepVarVals, &
        d_Toler, da_Betas, da_Resids, ia_Work, d_TempTheta, l_TrueLad)
i_ExitCode  = INT(da_Data(i_NumObsP2,i_NumVarsP1))
i_Iter      = INT(da_Data(i_NumObsP2,i_NumVarsP2))
d_SumAbsValRes =  da_Data(i_NumObsP1,i_NumVarsP1)
        ! Full Model, Quantile Regression
d_WtSumAbsDevsFulMod = d_ZERO
IF (.NOT.l_TrueLad) THEN ! For Quantile regression only ???????????
   DO i=1,i_NumObs
      IF (da_Resids(i) < d_ZERO) THEN ! if resid. is negative
         d_WtSumAbsDevsFulMod = d_WtSumAbsDevsFulMod + da_Resids(i)*(d_TempTheta-d_ONE)
      ELSE                            ! if resid. is positive
         d_WtSumAbsDevsFulMod = d_WtSumAbsDevsFulMod + da_Resids(i)*d_TempTheta
      END IF
   END DO
END IF

80000 CONTINUE
IF (ALLOCATED(da_Y         )) DEALLOCATE(da_Y,         STAT=ios)
IF (ALLOCATED(da_DataSave  )) DEALLOCATE(da_DataSave,  STAT=ios)
IF (ALLOCATED(da_DepVarVals)) DEALLOCATE(da_DepVarVals,STAT=ios)
IF (ALLOCATED(ia_Work      )) DEALLOCATE(ia_Work,      STAT=ios)
!d CALL MyDebug("<< outa LQLEAST")                                     ! debug!

RETURN
END SUBROUTINE lqleast

!==============================================================================

SUBROUTINE ftlqleast(i_NumObs, i_NumVars, d_Theta, i_NumPermut, &
                    i_RandNumSeed, d_Toler, l_DoLadSave, &
                    da_Data, &
                    i_CntSumAVR, d_PValue, &
                    da_Betas, da_Resids, &
                    d_SumAbsValRes, d_WtSumAbsDevsFulMod, &
                    i_Iter, i_ExitCode, iEr,ch_MyMsg)
!___Imported Parameters and Variables:
USE blcmnmod, ONLY: d_ONE, d_ZERO, i_NOT_OK, i_OK
        ! ^-- Some global Blossom variables and parameters.
USE jonsmodule, ONLY: i_CCode, i_REGSRMOD_ER001, i_REGSRMOD_ER011, ch_EM00033
        ! ^-- Message handling, etc.
!___Imported Procedures:
USE jonsmodule, ONLY: errhand
      
USE mt19937, ONLY: init_genrand
        ! ^-- Mersenne Twister Pseudo Random Number Generator
IMPLICIT NONE
!___Dummy Arguments:
INTEGER,          INTENT(IN)    :: i_NumObs
INTEGER,          INTENT(IN)    :: i_NumVars
REAL (KIND(0D0)), INTENT(IN)    :: d_Theta
INTEGER,          INTENT(IN)    :: i_NumPermut
INTEGER,          INTENT(IN)    :: i_RandNumSeed
REAL (KIND(0D0)), INTENT(IN)    :: d_Toler
LOGICAL,          INTENT(IN)    :: l_DoLadSave
REAL (KIND(0D0)), INTENT(INOUT) :: da_Data(:,:)
INTEGER,          INTENT(OUT)   :: i_CntSumAVR
REAL (KIND(0D0)), INTENT(OUT)   :: d_PValue
REAL (KIND(0D0)), INTENT(OUT)   :: da_Betas(:)
REAL (KIND(0D0)), INTENT(OUT)   :: da_Resids(:)
REAL (KIND(0D0)), INTENT(OUT)   :: d_SumAbsValRes
REAL (KIND(0D0)), INTENT(OUT)   :: d_WtSumAbsDevsFulMod
INTEGER,          INTENT(OUT)   :: i_Iter
INTEGER,          INTENT(OUT)   :: i_ExitCode
INTEGER,          INTENT(OUT)   :: iEr
INTEGER, PARAMETER :: i_LEN_MY_EMSG = 254 ! needs to be this big for output of var list in dostatus.
CHARACTER (LEN=i_LEN_MY_EMSG), INTENT(OUT) :: ch_MyMsg
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
!     SUBROUTINE FTLQLEAST
! DESCRIPTION:
!     Perform a lad or quantile regression with test of the "full" model.
!
! LAD / TEST [QUANT=t] .........................................(LAD_TEST)
!    Condition:  Command is LAD / TEST [ (also) /QUANT ]
!
!       Action:  Test of Full Model, essentially that Ho Y=constant
!
!                 -> L1 -> Resids -> |permut(resids)| ->
!                    (1)             \ (2...NPerms) /
!                                     \    loop    /
!                                      <-   L1   <-
!
!       Output: Betas,
!               P-value of Full Model    LEAST
!
! LANGUAGE:
!     Fortran 90/95
! AUTHOR:
!     Jon D. Richards, Operations Research Analyst    jon_richards@usgs.gov
!     Fort Collins Science Center                  http://www.fort.usgs.gov
!     U.S. Geological Survey  -  Biological Resources Division
! MODIFICATION HISTORY:
!   CODE HISTORY:
!     CREATED - JDR Jan 2004
!     Mod - Add option to use Mersenne Twister Pseudo Random Number Generator
!           Initialize both PRNGs at once. - JDR Apr 2005
!   COMMENT HISTORY:
!     CREATED - JDR Jan 2004!
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
!___Local Variables:
REAL (KIND(0D0)), ALLOCATABLE :: da_DataSave(:,:)
REAL (KIND(0D0)), ALLOCATABLE :: da_Y(:) ! copy of dep var vals
REAL (KIND(0D0)), ALLOCATABLE :: da_DepVarVals(:) ! dep var vals
REAL (KIND(0D0)), ALLOCATABLE :: da_BetasLocal(:)
REAL (KIND(0D0)), ALLOCATABLE :: da_ResidsLoc(:)!save residuals from first time through
REAL (KIND(0D0)) :: d_SumAbsValRes1st, d_TempTheta, d_WtSumAbsDevsFulMod1st
INTEGER, ALLOCATABLE :: ia_Work(:)
INTEGER :: i, i_Count, i_NumObsP1, i_NumObsP2, i_NumVarsP1, i_NumVarsP2, &
      ios, i_Sz
LOGICAL :: l_FirstTimeThru, l_TrueLad
!___Intrinsic Procedures:
INTRINSIC :: ALLOCATED, DBLE, HUGE, INT, RANDOM_SEED
!___Executable Statements:
!d CALL MyDebug(">> into FTLQLEAST")                                   ! debug!
                                ! debug!
iEr = i_OK ! no problem yet
i_Iter = 0
i_ExitCode = -HUGE(1) ! absurd value
d_PValue             = -HUGE(d_ONE) ! absurd value
d_SumAbsValRes       = -HUGE(d_ONE) ! absurd value
d_WtSumAbsDevsFulMod = -HUGE(d_ONE) ! absurd value
! Initialize default PRNG
CALL RANDOM_SEED(SIZE=i_Sz)


CALL RANDOM_SEED(PUT=(/(i_RandNumSeed,i=1,i_Sz)/))


! Initialize Mersenne Twister PRNG
CALL init_genrand(i_RandNumSeed)
IF (d_Theta < d_ZERO) THEN
   l_TrueLad = .TRUE.
   d_TempTheta = -d_Theta
ELSE
   d_TempTheta = d_Theta
   l_TrueLad = .FALSE.
END IF
i_NumObsP1  = i_NumObs  + 1
i_NumVarsP1 = i_NumVars + 1
i_NumObsP2  = i_NumObs  + 2
i_NumVarsP2 = i_NumVars + 2
ALLOCATE(ia_Work(1:i_NumObs), &
            da_Y(1:i_NumObs), &
   da_DepVarVals(1:i_NumObs), &
     da_DataSave(1:i_NumObsP1,1:i_NumVarsP1), &
   da_BetasLocal(1:i_NumVars), &
   da_ResidsLoc(1:i_NumObs), &
            STAT=ios)
IF (ios /= i_OK) THEN ! Memory allocation error
   CALL errhand(ch_MyMsg,i_Code=ios, &
                ch_Msg=ch_EM00033, &
                ch_S1="dynamic arrays", &
                ch_S2="ftlqleast")
   iEr = i_NOT_OK
   GOTO 80000
END IF
da_DataSave(1:i_NumObs,1:i_NumVarsP1) = da_Data(1:i_NumObs,1:i_NumVarsP1)
da_DepVarVals(1:i_NumObs) = da_Data(1:i_NumObs,i_NumVarsP1) ! copy to send L1
da_Y(1:i_NumObs) = da_DepVarVals(1:i_NumObs) ! we will permute these
l_FirstTimeThru = .TRUE. ! first time through permutation loop
i_CntSumAVR = 0
! ... B E G I N   P e r m u t a t i o n   L o o p ...  ... ... ... ... !
PermutationLoop: DO i_Count=1,i_NumPermut
   IF (l_FirstTimeThru) THEN
      da_DataSave(1:i_NumObs,1:i_NumVarsP1) = da_Data(1:i_NumObs,1:i_NumVarsP1) ! keep copy
      da_DepVarVals(1:i_NumObs) = da_Data(1:i_NumObs,i_NumVarsP1)   ! keep copies of these
   ELSE  ! Second and subsequent passes through permutation loop
      CALL permut(da_Y, i_NumObs)     ! permute the m values in da_Y matrix
      da_DepVarVals(1:i_NumObs) = da_Y(1:i_NumObs) ! for full model tests..
   END IF
   CALL L1(i_NumObs, i_NumVars, i_NumObsP2, i_NumVarsP2, da_Data, &
           da_DepVarVals, d_Toler, da_BetasLocal, da_Resids, ia_Work, &
           d_TempTheta, l_TrueLad)
   IF (l_FirstTimeThru) THEN
      da_ResidsLoc(1:i_NumObs)=da_Resids
      da_Betas(1:i_NumVars) = da_BetasLocal(1:i_NumVars)
      i_ExitCode  = INT(da_Data(i_NumObsP2,i_NumVarsP1))
      i_Iter      = INT(da_Data(i_NumObsP2,i_NumVarsP2))
   END IF ! (l_FirstTimeThru)
   d_SumAbsValRes = da_Data(i_NumObsP1,i_NumVarsP1)
   d_WtSumAbsDevsFulMod = d_ZERO
   IF (.NOT.l_TrueLad) THEN ! Quantile Regression
      DO i=1,i_NumObs
         IF (da_Resids(i) < d_ZERO) THEN    ! if resid. is negative
            d_WtSumAbsDevsFulMod = d_WtSumAbsDevsFulMod + da_Resids(i)*(d_TempTheta-d_ONE)
         ELSE                      ! if resid. is positive !
            d_WtSumAbsDevsFulMod = d_WtSumAbsDevsFulMod + da_Resids(i)*d_TempTheta
         END IF
      END DO
   END IF ! (l_DoQuantRegression)
   IF (l_FirstTimeThru) THEN
      d_SumAbsValRes1st = d_SumAbsValRes
      IF (.NOT.l_TrueLad) d_WtSumAbsDevsFulMod1st = d_WtSumAbsDevsFulMod
      i_CntSumAVR = i_CntSumAVR + 1  ! full model to compare later

   ELSE ! ................. second and subsequent permutations:
      ! if latest sums of the abs value of residuals is less than the sums
      ! of the abs val of resids for first unpermuted regression, accumulate
      IF (l_TrueLad) THEN
         IF (d_SumAbsValRes <= d_SumAbsValRes1st) i_CntSumAVR = i_CntSumAVR + 1
      ELSE ! Quantile regression
         IF (d_WtSumAbsDevsFulMod <= d_WtSumAbsDevsFulMod1st) i_CntSumAVR = i_CntSumAVR + 1
      END IF
   END IF ! (l_FirstTimeThru/2nd..)
   IF (l_FirstTimeThru) l_FirstTimeThru = .FALSE.
   da_Data(1:i_NumObs,1:i_NumVarsP1) = da_DataSave(1:i_NumObs,1:i_NumVarsP1)
END DO PermutationLoop
da_Resids=da_ResidsLoc
! ...  E N D   P e r m u t a t i o n   L o o p ... ... ... ... ... ... !
d_PValue = DBLE(i_CntSumAVR)/DBLE(i_NumPermut) ! P-value of the Full Model.
d_SumAbsValRes = d_SumAbsValRes1st ! from first regression.
IF (.NOT.l_TrueLad) d_WtSumAbsDevsFulMod = d_WtSumAbsDevsFulMod1st ! from first regression.
80000 CONTINUE
IF (ALLOCATED(da_ResidsLoc)) DEALLOCATE(da_ResidsLoc,STAT=ios)
IF (ALLOCATED(da_BetasLocal)) DEALLOCATE(da_BetasLocal,STAT=ios)
IF (ALLOCATED(da_DataSave  )) DEALLOCATE(da_DataSave,  STAT=ios)
IF (ALLOCATED(da_DepVarVals)) DEALLOCATE(da_DepVarVals,STAT=ios)
IF (ALLOCATED(da_Y         )) DEALLOCATE(da_Y,         STAT=ios)
IF (ALLOCATED(ia_Work      )) DEALLOCATE(ia_Work,      STAT=ios)
RETURN
END SUBROUTINE ftlqleast

!==============================================================================

SUBROUTINE htlqleast(i_NumObs, i_NumVars, d_Theta, i_NumPermut, &
                    i_RandNumSeed, i_NumToDrop, ia_PosToDrop, d_Toler, &
                    da_Data, &
                    d_To, d_PValue, &
                    da_Betas, da_RedBetas, &
                    da_Resids, da_RedResids, &
                    d_SumAbsValRes, d_SumAbsValResRed, &
                    d_WtSumAbsDevsFulMod, d_WtSumAbsDevsRedMod, &
                    i_Iter, i_ExitCode, iEr,ch_MyMsg,da_STV,l_SaveTest)
!___Imported Parameters and Variables:

USE blcmnmod, ONLY: d_ONE, d_ZERO, i_NOT_OK, i_OK
        ! ^-- Some global Blossom variables and parameters.
USE jonsmodule, ONLY: i_CCode, i_REGSRMOD_ER001, i_REGSRMOD_ER011, ch_EM00033, &
      ch_REGRSMOD_EM021
        ! ^-- Message handling, etc.
!___Imported Procedures:
USE jonsmodule, ONLY: errhand
      
USE mt19937, ONLY: init_genrand
        ! ^-- Mersenne Twister Pseudo Random Number Generator
IMPLICIT NONE
!___Dummy Arguments:
INTEGER,          INTENT(IN)    :: i_NumObs
INTEGER,          INTENT(IN)    :: i_NumVars
REAL (KIND(0D0)), INTENT(IN)    :: d_Theta
INTEGER,          INTENT(IN)    :: i_NumPermut
INTEGER,          INTENT(IN)    :: i_RandNumSeed
INTEGER,          INTENT(IN)    :: i_NumToDrop
INTEGER,          INTENT(IN)    :: ia_PosToDrop(:)
REAL (KIND(0D0)), INTENT(IN)    :: d_Toler
REAL (KIND(0D0)), INTENT(INOUT) :: da_Data(:,:)
REAL (KIND(0D0)), INTENT(OUT)   :: d_To
REAL (KIND(0D0)), INTENT(OUT)   :: d_PValue
REAL (KIND(0D0)), INTENT(OUT)   :: da_Betas(:)
REAL (KIND(0D0)), INTENT(OUT)   :: da_RedBetas(:)
REAL (KIND(0D0)), INTENT(OUT)   :: da_Resids(:)
REAL (KIND(0D0)), INTENT(OUT)   :: da_RedResids(:)
REAL (KIND(0D0)), INTENT(OUT)   :: d_SumAbsValRes
REAL (KIND(0D0)), INTENT(OUT)   :: d_SumAbsValResRed
REAL (KIND(0D0)), INTENT(OUT)   :: d_WtSumAbsDevsFulMod
REAL (KIND(0D0)), INTENT(OUT)   :: d_WtSumAbsDevsRedMod
INTEGER,          INTENT(OUT)   :: i_Iter
INTEGER,          INTENT(OUT)   :: i_ExitCode
INTEGER,          INTENT(OUT)   :: iEr
INTEGER, PARAMETER :: i_LEN_MY_EMSG = 254 ! needs to be this big for output of var list in dostatus.
CHARACTER (LEN=i_LEN_MY_EMSG), INTENT(OUT) :: ch_MyMsg
REAL (KIND(0D0)), INTENT(OUT)    :: da_STV(:)
LOGICAL,          INTENT(IN)     :: l_SaveTest
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
!     SUBROUTINE HTLQLEAST
! DESCRIPTION:
!     Perform hypothesis test of lad or quantile regression, return results of
!     reduced model output and test results.
!
! HYP (lad/quant) .........................................(HYP_LAD_QUANT)
!    Condition:  1. LAD command given 1st (may have /QUANT)
!                2. HYP command with reduced model
!                3. Not Rank Score, Not Double Permutation.
!                4. Only one parameter in reduced model.
!
!       Action:  Permute residuals from original (Null Ho) Hypothesis
!                model.
!
!                LAD ->
!                -> L1 -> Resids (with XData) -> |permut(resids)| ->
!                   (1)                          \ (2...NPerms) /
!                                                 \    loop    /
!                                                  <-   L1   <-
!
!
!       Output:  Betas,
!                Sum of absolute values of the residuals,
!                Weighted sum of the absolute deviations,
!                Observed Test Statistic,
!                P-value of variables in full model but not in reduced
!                        model
!
! LANGUAGE:
!     Fortran 90/95
! AUTHOR:
!     Jon D. Richards, Operations Research Analyst    jon_richards@usgs.gov
!     Fort Collins Science Center                  http://www.fort.usgs.gov
!     U.S. Geological Survey  -  Biological Resources Division
! MODIFICATION HISTORY:
!   CODE HISTORY:
!     CREATED - JDR Jan 2004
!     Mod - Added option to have SAVETEST of regression test statistics for
!           original and permuted regressions to a file - JDR  24 Mar 2005
!     Mod - Add option to use Mersenne Twister Pseudo Random Number Generator
!           Initialize both PRNGs at once. - JDR Apr 2005
!   COMMENT HISTORY:
!     CREATED - JDR Jan 2004!
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
!___Local Parameters:
! REAL (KIND(0D0)), PARAMETER :: d_SMALL_VALUE = 1.0D-015
!___Local Variables:
REAL (KIND(0D0)), ALLOCATABLE :: da_DataRed(:,:) ! reduced data array
REAL (KIND(0D0)), ALLOCATABLE :: da_DataSave(:,:)
REAL (KIND(0D0)), ALLOCATABLE :: da_DataSaveRed(:,:) ! copy of red data array
REAL (KIND(0D0)), ALLOCATABLE :: da_DepVarVals(:) ! copy of dep var vals
REAL (KIND(0D0)), ALLOCATABLE :: da_BetasLocal(:)
REAL (KIND(0D0)), ALLOCATABLE :: da_DepVarValsRed(:) ! reduced dep var vals
REAL (KIND(0D0)), ALLOCATABLE :: da_ResSaveRed(:) ! copy of red Mod resids
REAL (KIND(0D0)), ALLOCATABLE :: da_RedBetasLocal(:) ! betas red mod local
REAL (KIND(0D0)), ALLOCATABLE :: da_YRed(:) ! copy red Mod vals
 ! save test valuesLo
REAL (KIND(0D0)) :: d_SumAbsValRes1st, d_SumAbsValResRed1st, d_TempTheta, &
      d_Tperm, d_WtSumAbsDevsRedMod1st
INTEGER, ALLOCATABLE :: ia_Work(:)
INTEGER :: i, i_Count, i_JCount, i_NumObsP1, i_NumObsP2, i_NumVarsP1, &
      i_NumVarsP2, i_NumVRed, i_NumVRedP1, i_NumVRedP2, ios, i_Sz, &
      i_ToCount, j,i_SaveTest
LOGICAL :: l_FirstTimeThru, l_HaveZero, l_TrueLad
!d INTEGER :: iii, jjj                                                 ! debug!
!___Intrinsic Procedures:
INTRINSIC :: ALLOCATED, DBLE, HUGE, INT, RANDOM_SEED
!___Executable Statements:
!d CALL MyDebug(">> into HTLQLEAST")

i_SaveTest=1                                  ! debug!
iEr = i_OK ! no problem yet
i_Iter = 0
i_ExitCode = -HUGE(1) ! absurd value
d_To                 = -HUGE(d_ONE) ! absurd value
d_PValue             = -HUGE(d_ONE) ! absurd value
d_SumAbsValRes       = -HUGE(d_ONE) ! absurd value
d_SumAbsValResRed    = -HUGE(d_ONE) ! absurd value
d_WtSumAbsDevsFulMod = -HUGE(d_ONE) ! absurd value
d_WtSumAbsDevsRedMod = -HUGE(d_ONE) ! absurd value
! Initialize default PRNG
CALL RANDOM_SEED(SIZE=i_Sz)
CALL RANDOM_SEED(PUT=(/(i_RandNumSeed,i=1,i_Sz)/))
! Initialize Mersenne Twister PRNG
CALL init_genrand(i_RandNumSeed)
IF (d_Theta < d_ZERO) THEN
   l_TrueLad = .TRUE.
   d_TempTheta = -d_Theta
ELSE
   d_TempTheta = d_Theta
   l_TrueLad = .FALSE.
END IF
l_HaveZero = .FALSE. ! initial non-0 sums of abs vals of resids
i_JCount = 1
i_NumObsP1  = i_NumObs  + 1
i_NumVarsP1 = i_NumVars + 1
i_NumObsP2  = i_NumObs  + 2
i_NumVarsP2 = i_NumVars + 2
i_NumVRed   = i_NumVars - i_NumToDrop
i_NumVRedP1 = i_NumVRed + 1
i_NumVRedP2 = i_NumVRed + 2
ALLOCATE(ia_Work(1:i_NumObs),                 &
      da_DataRed(1:i_NumObsP2,1:i_NumVRedP2), & ! reduced data array
     da_DataSave(1:i_NumObsP1,1:i_NumVarsP1), &
  da_DataSaveRed(1:i_NumObsP2,1:i_NumVarsP2), & ! copy of reduced data array
   da_DepVarVals(1:i_NumObs),                 &
   da_BetasLocal(1:i_NumVars),                &
da_DepVarValsRed(1:i_NumObs),                 & ! reduced dep var vals
   da_ResSaveRed(1:i_NumObs),                 & ! copy of red Mod resids
da_RedBetasLocal(1:i_NumVRed),                & ! betas of red mod local
         da_YRed(1:i_NumObs),                 & ! copy red Mod vals
         STAT=ios)
IF (ios /= i_OK) THEN ! Memory allocation error
   CALL errhand(ch_MyMsg,i_Code=ios, &
                ch_Msg=ch_EM00033, &
                ch_S1="dynamic arrays", &
                ch_S2="htlqleast")
   iEr = i_NOT_OK
   GOTO 80000
END IF
        ! Fill a data matrix for reduced model. Also, remember that
        ! the last column in data matrix is dependent variable.
        ! If we have a reduced model, we will drop some variables.
DO j=1,i_NumVarsP1
   IF (j == i_NumVarsP1) THEN
      da_DataRed(1:i_NumObs,i_JCount) = da_Data(1:i_NumObs,j) ! this is dependent var
   ELSE
      IF (ia_PosToDrop(j) == 0) THEN
         da_DataRed(1:i_NumObs,i_JCount) = da_Data(1:i_NumObs,j) ! indep var values
         i_JCount = i_JCount + 1
      END IF
   END IF
END DO
l_FirstTimeThru      = .TRUE. ! first time through permutation loop
d_WtSumAbsDevsRedMod = d_ZERO
d_WtSumAbsDevsFulMod = d_ZERO
d_SumAbsValResRed    = d_ZERO

        ! ... B E G I N   P e r m u t a t i o n   L o o p ...  ... ... ... ... !
PermutationLoop: DO i_Count=1,i_NumPermut

   IF (l_FirstTimeThru) THEN

      da_DataSave(1:i_NumObs,1:i_NumVarsP1) = da_Data(1:i_NumObs,1:i_NumVarsP1)
      da_DataSaveRed(1:i_NumObs,1:i_NumVRedP1) = da_DataRed(1:i_NumObs,1:i_NumVRedP1)

      da_DepVarVals(1:i_NumObs) = da_Data(1:i_NumObs,i_NumVarsP1)
      da_DepVarValsRed(1:i_NumObs) = da_DataRed(1:i_NumObs,i_NumVRedP1)
      da_YRed(1:i_NumObs) = da_DepVarValsRed(1:i_NumObs)
   ELSE  ! Second and subsequent passes through permutation loop
      CALL permut(da_ResSaveRed,i_NumObs) ! permute the resids
      da_DepVarValsRed(1:i_NumObs) = da_ResSaveRed(1:i_NumObs)
      da_DepVarVals(1:i_NumObs) = da_ResSaveRed(1:i_NumObs)
   END IF
!--------------------- Begin calls to L1 routine --------------------- !
   CALL L1(i_NumObs, i_NumVars, i_NumObsP2, i_NumVarsP2, da_Data,   &
           da_DepVarVals,    d_Toler, da_BetasLocal,     da_Resids, &
           ia_Work, d_TempTheta, l_TrueLad)
   IF (l_FirstTimeThru) da_Betas = da_BetasLocal
   CALL L1(i_NumObs, i_NumVRed, i_NumObsP2, i_NumVRedP2, da_DataRed,   &
           da_DepVarValsRed, d_Toler, da_RedBetasLocal,  da_RedResids, &
           ia_Work, d_TempTheta, l_TrueLad)
   IF (l_FirstTimeThru) da_RedBetas(1:i_NumVRed) = da_RedBetasLocal(1:i_NumVRed)
!----------------------- End calls to L1 routine --------------------- !
   IF (l_FirstTimeThru) THEN
      i_ExitCode = INT(da_DataRed(i_NumObsP2,i_NumVRedP1))
      i_Iter     = INT(da_DataRed(i_NumObsP2,i_NumVRedP2))
   END IF ! (l_FirstTimeThru)
   d_SumAbsValRes    = da_Data(i_NumObsP1,i_NumVarsP1) ! FOR FULL MODEL
   d_SumAbsValResRed = da_DataRed(i_NumObsP1,i_NumVRedP1) ! same FOR REDUCED MODEL
        !----July 13 1998 new code to properly accumulate Weighted sums of the
        ! absolute deviations for *QUANTILE Regressions*==>(Quant is positive)
   IF (.NOT.l_TrueLad) THEN ! Quantile Regression
      d_WtSumAbsDevsRedMod = d_ZERO
      DO i=1,i_NumObs
         IF (da_RedResids(i) < d_ZERO) THEN ! if resid. is negative
            d_WtSumAbsDevsRedMod = d_WtSumAbsDevsRedMod + da_RedResids(i)*(d_TempTheta-d_ONE)
         ELSE                      ! if resid. is positive
            d_WtSumAbsDevsRedMod = d_WtSumAbsDevsRedMod + da_RedResids(i)*d_TempTheta
         END IF
      END DO
                  ! Full Model, Quantile Regression
      d_WtSumAbsDevsFulMod = d_ZERO
      DO i=1,i_NumObs
         IF (da_Resids(i) < d_ZERO) THEN    ! if resid. is negative
            d_WtSumAbsDevsFulMod = d_WtSumAbsDevsFulMod + da_Resids(i)*(d_TempTheta-d_ONE)
         ELSE                      ! if resid. is positive !
            d_WtSumAbsDevsFulMod = d_WtSumAbsDevsFulMod + da_Resids(i)*d_TempTheta
         END IF
      END DO
   END IF ! Quantile Regression
        !---- end July 13 1998 code segment -------------------------------- !
   IF (l_FirstTimeThru) THEN     !..... FIRST (unpermuted) regression
      d_SumAbsValRes1st    = d_SumAbsValRes
      d_SumAbsValResRed1st = d_SumAbsValResRed

      d_WtSumAbsDevsRedMod1st = d_WtSumAbsDevsRedMod
        ! if d_SumAbsValRes = 0 then we don't want to divide by it.  In fact if the
        ! sum of the absolute values of the residuals is zero, we have a
        ! perfect fit and the P-value is 1.0; we don't need to go through the
        ! contortions of the permutations.  We can just put out our P-value
        ! and put out "Undefined" as value of the test statistic d_To_LQ
        ! (aka Tee-zero).
      IF (l_TrueLad) THEN
         IF (d_SumAbsValRes <= d_SMALL_VALUE) THEN
            l_HaveZero = .TRUE.    ! Don't divide by Zero... a
            i_ToCount = 1 ! ??? count here ???
            EXIT PermutationLoop   ! truly pathological escape
         ELSE       ! TestStat!
            d_To = (d_SumAbsValResRed-d_SumAbsValRes) / d_SumAbsValRes
            i_ToCount = 1
         END IF
      ELSE ! not a true LAD, a quantile regression
         IF (d_WtSumAbsDevsFulMod <= d_SMALL_VALUE) THEN
            l_HaveZero = .TRUE.    ! Don't divide by Zero... a
            i_ToCount = 1 ! ??? count here ???
            EXIT PermutationLoop   ! truly pathological escape
         ELSE
            d_To = (d_WtSumAbsDevsRedMod-d_WtSumAbsDevsFulMod) / d_WtSumAbsDevsFulMod
            i_ToCount = 1
         END IF
      END IF
      IF (l_SAVETEST) THEN  ! Save value of test stat for this permuted sample
         da_STV(i_Count) = d_To
      END IF
      da_ResSaveRed(1:i_NumObs) = da_RedResids(1:i_NumObs)
   ELSE ! ................. second and subsequent permutations:
        ! if this difference is less than the difference for the unpermuted
        ! first regression, accumulate statistic ToCount
        !.......... IF (tests) THEN accumulate test statistic To
      IF (l_TrueLad) THEN  ! if true lad regression
         d_Tperm = (d_SumAbsValResRed-d_SumAbsValRes) / d_SumAbsValRes
      ELSE                ! else Quantile regression
         d_Tperm = (d_WtSumAbsDevsRedMod-d_WtSumAbsDevsFulMod) / d_WtSumAbsDevsFulMod
      END IF
      IF (l_SAVETEST) THEN  ! Save value of test stat for this permuted sample
         da_STV(i_Count) = d_Tperm
      END IF

      IF (d_Tperm >= d_To) i_ToCount = i_ToCount + 1
   END IF ! (l_FirstTimeThru/2nd..)
   IF (l_FirstTimeThru) l_FirstTimeThru = .FALSE.
   IF (l_HaveZero) EXIT PermutationLoop ! sum abs resids=0; exact fit, skip
   IF (i_NumPermut > 1) THEN ! Refresh observed
      da_Data(1:i_NumObs,1:i_NumVarsP1) = da_DataSave(1:i_NumObs,1:i_NumVarsP1)
      da_DataRed(1:i_NumObs,1:i_NumVRedP1) = da_DataSaveRed(1:i_NumObs,1:i_NumVRedP1)
IF (i_Count == 2000) THEN

END IF
   END IF
END DO PermutationLoop
        ! ...  E N D   P e r m u t a t i o n   L o o p ... ... ... ... ... ... !

! /////////////////////////////////////////////////////////////////////////////////////////////
!
! Check to see that we have access to output file
!
! /////////////////////////////////////////////////////////////////////////////////////////////



IF (l_HaveZero) THEN ! Have ZERO residuals in the reduced model!
   d_PValue = d_ONE
   d_SumAbsValResRed = d_ZERO
!  _______________________________________________
! |                                               |
! | Perfect fit, zero residuals in reduced model. |
! | We can't test this any further.               |
! |_______________________________________________|
!
   CALL errhand(ch_MyMsg,ch_Msg=ch_REGRSMOD_EM021)
   iEr = i_NOT_OK
ELSE
   d_PValue = DBLE(i_ToCount)/DBLE(i_NumPermut)
END IF
iEr = i_OK
d_SumAbsValRes    = d_SumAbsValRes1st
d_SumAbsValResRed = d_SumAbsValResRed1st
d_WtSumAbsDevsRedMod = d_WtSumAbsDevsRedMod1st
80000 CONTINUE
IF (ALLOCATED(da_YRed         )) DEALLOCATE(da_YRed,         STAT=ios)
IF (ALLOCATED(da_RedBetasLocal)) DEALLOCATE(da_RedBetasLocal,STAT=ios)
IF (ALLOCATED(da_ResSaveRed   )) DEALLOCATE(da_ResSaveRed,   STAT=ios)
IF (ALLOCATED(da_DepVarValsRed)) DEALLOCATE(da_DepVarValsRed,STAT=ios)
IF (ALLOCATED(da_BetasLocal   )) DEALLOCATE(da_BetasLocal,   STAT=ios)
IF (ALLOCATED(da_DepVarVals   )) DEALLOCATE(da_DepVarVals,   STAT=ios)
IF (ALLOCATED(da_DataSaveRed  )) DEALLOCATE(da_DataSaveRed,  STAT=ios)
IF (ALLOCATED(da_DataSave     )) DEALLOCATE(da_DataSave,     STAT=ios)
IF (ALLOCATED(da_DataRed      )) DEALLOCATE(da_DataRed,      STAT=ios)
IF (ALLOCATED(ia_Work         )) DEALLOCATE(ia_Work,         STAT=ios)
RETURN   ! ... normal exit ...
END SUBROUTINE htlqleast

!==============================================================================

SUBROUTINE htqm1least(i_NumObs, i_NumVars, d_Theta, i_NumPermut, &
                      i_RandNumSeed, i_NumToDrop, ia_PosToDrop, d_Toler, &
                      da_Data, &
                      d_To, d_PValue, &
                      da_Betas, da_RedBetas, &
                      da_Resids, da_RedResids, &
                      d_SumAbsValResRed, d_WtSumAbsDevsRedMod, &
                      i_Iter, i_ExitCode, iEr,ch_MyMsg,da_STV,l_SaveTest)
!___Imported Parameters and Variables:

        ! ^-- Misc. Blossom file names, units, status parameters.
USE blcmnmod, ONLY: d_ONE, d_ZERO, i_NOT_OK, i_OK
        ! ^-- Some global Blossom variables and parameters.
USE jonsmodule, ONLY: ch_EM00033, ch_REGRSMOD_EM020
        ! ^-- Message handling, etc.
!___Imported Procedures:
USE jonsmodule, ONLY: errhand
       
USE mt19937, ONLY: init_genrand
        ! ^-- Mersenne Twister Pseudo Random Number Generator
IMPLICIT NONE
!___Dummy Arguments:
INTEGER,          INTENT(IN)    :: i_NumObs
INTEGER,          INTENT(IN)    :: i_NumVars
REAL (KIND(0D0)), INTENT(IN)    :: d_Theta
INTEGER,          INTENT(IN)    :: i_NumPermut
INTEGER,          INTENT(IN)    :: i_RandNumSeed
INTEGER,          INTENT(IN)    :: i_NumToDrop
INTEGER,          INTENT(IN)    :: ia_PosToDrop(:)
REAL (KIND(0D0)), INTENT(IN)    :: d_Toler
REAL (KIND(0D0)), INTENT(INOUT) :: da_Data(:,:)
REAL (KIND(0D0)), INTENT(OUT)   :: d_To
REAL (KIND(0D0)), INTENT(OUT)   :: d_PValue
REAL (KIND(0D0)), INTENT(OUT)   :: da_Betas(:)
REAL (KIND(0D0)), INTENT(OUT)   :: da_RedBetas(:)
REAL (KIND(0D0)), INTENT(OUT)   :: da_Resids(:)
REAL (KIND(0D0)), INTENT(OUT)   :: da_RedResids(:)
REAL (KIND(0D0)), INTENT(OUT)   :: d_SumAbsValResRed
REAL (KIND(0D0)), INTENT(OUT)   :: d_WtSumAbsDevsRedMod
INTEGER,          INTENT(OUT)   :: i_Iter
INTEGER,          INTENT(OUT)   :: i_ExitCode
INTEGER,          INTENT(OUT)   :: iEr
INTEGER, PARAMETER :: i_LEN_MY_EMSG = 254 ! needs to be this big for output of var list in dostatus.
CHARACTER (LEN=i_LEN_MY_EMSG), INTENT(OUT) :: ch_MyMsg
REAL (KIND(0D0)), INTENT(OUT)  :: da_STV(:) ! save test values
LOGICAL,          INTENT(IN)     :: l_SaveTest
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
!     SUBROUTINE HTQM1LEAST
! DESCRIPTION:
!     Perform hypothesis test of lad or quantile regression where the number
!     of parameters in the reduced model is greater than 1 and there is no
!     specification of rank score or double permutation.  Drop appropriate
!     number of observations with zero-valued residuals for the permutation
!     test, and report results of the reduced model and test.
!
! HYP (Deletion of Zero Valued Residuals) ................(HYP_DELETE_QM1)
!    Condition:  1. LAD command given 1st (may have /QUANT)
!                2. HYP command with reduced model
!                3. Not Rank Score, Not Double Permutation.
!                4. Number of parameters in reduced model > 1
!
!       Action:  Deletion of some zero-valued residuals and same
!                number of cases from the XData matrix. Different
!                cases are removed each iteration and the residuals
!                are permuted. Q = Number parameters (vars) in
!                Reduced model. Drop (Q-1) 0-valued resids.
!
!                LAD ->
!                    -> L1 -> Resids -> Drop (q-1) 0-valued resids -> ...
!                       (1)
!
!                ... -> |Permut(reduced number of residuals)| ->
!                       |Random Drop (q-1) cases from XData |
!                       | design matrix with permuted resids|
!                                   \ (2...NPerms) /
!                                    \    loop    /
!                                     <-   L1   <-
!
!       Output:  Betas,
!                Observed Test Statistic,
!                Sum of absolute values of the residuals,
!                Weighted sum of the absolute deviations,
!                P-value of variables in full model but not in reduced
!                        model
!
! LANGUAGE:
!     Fortran 90/95
! AUTHOR:
!     Jon D. Richards, Operations Research Analyst    jon_richards@usgs.gov
!     Fort Collins Science Center                  http://www.fort.usgs.gov
!     U.S. Geological Survey  -  Biological Resources Division
! MODIFICATION HISTORY:
!   CODE HISTORY:
!     CREATED - JDR Jan 2004
!     Mod - Added option to have SAVETEST of regression test statistics for
!           original and permuted regressions to a file - JDR  24 Mar 2005
!     Mod - Add option to use Mersenne Twister Pseudo Random Number Generator
!           Initialize both PRNGs at once. - JDR Apr 2005
!   COMMENT HISTORY:
!     CREATED - JDR Jan 2004!
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
!___Local Parameters:
! REAL (KIND(0D0)), PARAMETER :: d_LARGE_VALUE = 1.0D300
! REAL (KIND(0D0)), PARAMETER :: d_SMALL_VALUE = 1.0D-015
!___Local Variables:
REAL (KIND(0D0)), ALLOCATABLE :: da_DataQM1(:,:)
REAL (KIND(0D0)), ALLOCATABLE :: da_DataRed(:,:)
REAL (KIND(0D0)), ALLOCATABLE :: da_DataSave(:,:)
REAL (KIND(0D0)), ALLOCATABLE :: da_DepVarVals(:)
REAL (KIND(0D0)), ALLOCATABLE :: da_DepVarValsRed(:)
REAL (KIND(0D0)), ALLOCATABLE :: da_RedBetasLocal(:)
REAL (KIND(0D0)), ALLOCATABLE :: da_RedResidsQM1(:)
REAL (KIND(0D0)), ALLOCATABLE :: da_ResidsQM1(:)
REAL (KIND(0D0)), ALLOCATABLE :: da_RedResids1st(:)
REAL (KIND(0D0)), ALLOCATABLE :: da_ResRedDandP(:)
REAL (KIND(0D0)), ALLOCATABLE :: da_BetasLocal(:)
REAL (KIND(0D0)) :: d_SumAbsValRes, d_SumAbsValResRed1st, d_TempTheta, &
      d_To_LQ, d_WtSumAbsDevsFulMod, d_WtSumAbsDevsRedModLocal
INTEGER, ALLOCATABLE :: ia_DropZero(:)
INTEGER, ALLOCATABLE :: ia_Work(:)
INTEGER :: i, i1, i_Cnt, i_Cnt2Perm, i_JCount, i_NumObsP1, i_NumObsP2, &
      i_NumObsQM1, i_NumObsQM1P1, i_NumObsQM1P2, i_NumVarsP1, i_NumVarsP2, &
      i_NumVRed, i_NumVRedP1, i_NumVRedP2, i_Qm1, ios, i_Sz, j
LOGICAL :: l_HaveZero, l_TrueLad
!___Intrinsic Procedures:
INTRINSIC :: ALLOCATED, DBLE, HUGE, INT, RANDOM_SEED
!___Executable Statements:
!d CALL MyDebug(">> into HTQM1LEAST")                                  ! debug!
!d l_ON = .FALSE.                                                      ! debug!
iEr = i_OK ! no problem yet
i_Iter = 0
i_ExitCode = -HUGE(1) ! absurd value
d_To                 = -HUGE(d_ONE) ! absurd value
d_PValue             = -HUGE(d_ONE) ! absurd value
d_SumAbsValResRed    = -HUGE(d_ONE) ! absurd value
d_WtSumAbsDevsRedMod = -HUGE(d_ONE) ! absurd value

! Initialize default PRNG
CALL RANDOM_SEED(SIZE=i_Sz)
CALL RANDOM_SEED(PUT=(/(i_RandNumSeed,i=1,i_Sz)/))
! Initialize Mersenne Twister PRNG
CALL init_genrand(i_RandNumSeed)
IF (d_Theta < d_ZERO) THEN
   l_TrueLad = .TRUE.
   d_TempTheta = -d_Theta
ELSE
   d_TempTheta = d_Theta
   l_TrueLad = .FALSE.
END IF
i_NumVarsP1   = i_NumVars + 1
i_NumVarsP2   = i_NumVars + 2
i_NumObsP1    = i_NumObs  + 1
i_NumObsP2    = i_NumObs  + 2
i_NumVRed     = i_NumVars - i_NumToDrop
i_NumVRedP1   = i_NumVRed + 1
i_NumVRedP2   = i_NumVRed + 2
i_Qm1         = i_NumVRed - 1
i_NumObsQM1   = i_NumObs  - i_Qm1
i_NumObsQM1P1 = i_NumObsQM1 + 1
i_NumObsQM1P2 = i_NumObsQM1 + 2
ALLOCATE(da_RedResids1st(1:i_NumObs),  &
        da_RedBetasLocal(1:i_NumVRed), &
           da_BetasLocal(1:i_NumVars), &
                 ia_Work(1:i_NumObs),  &
             da_DataSave(1:i_NumObsP2,1:i_NumVarsP2), &
           da_DepVarVals(1:i_NumObs),  &
        da_DepVarValsRed(1:i_NumObs),  &
              da_DataRed(1:i_NumObsP2,1:i_NumVRedP2), &
            STAT=ios)
IF (ios /= i_OK) THEN
   CALL errhand(ch_MyMsg,i_Code=ios, &
                ch_Msg=ch_EM00033, &
                ch_S1="dynamic arrays", &
                ch_S2="htqm1least")
   iEr = i_NOT_OK
   GOTO 80000
END IF
da_DataSave(1:i_NumObs,1:i_NumVarsP1) = da_Data(1:i_NumObs,1:i_NumVarsP1)
l_HaveZero = .FALSE. ! initial non-0 sums of abs vals of resids
! Fill an da_Data data matrix for reduced model. Also, remember that
! the last column in da_Data matrix is dependent variable.
! If we have a reduced model, we will drop some variables.
i_JCount = 1
DO j=1,i_NumVarsP1
   IF (j == i_NumVarsP1) THEN
      da_DataRed(1:i_NumObs,i_JCount) = da_Data(1:i_NumObs,j) ! this is dependent var
   ELSE
      IF (ia_PosToDrop(j) == 0) THEN
         da_DataRed(1:i_NumObs,i_JCount) = da_Data(1:i_NumObs,j) ! indep var values
         i_JCount = i_JCount + 1
      END IF
   END IF
END DO
d_SumAbsValResRed    = d_ZERO
da_DepVarVals(1:i_NumObs) = da_Data(1:i_NumObs,i_NumVarsP1) ! keep copies
da_DepVarValsRed(1:i_NumObs) = da_DataRed(1:i_NumObs,i_NumVRedP1)
!--------------------- Begin calls to L1 routine --------------------- !
CALL L1(i_NumObs, i_NumVars, i_NumObsP2, i_NumVarsP2, da_Data, &
      da_DepVarVals,    d_Toler, da_BetasLocal,    da_Resids,  &
      ia_Work, d_TempTheta, l_TrueLad)
da_Betas = da_BetasLocal
CALL L1(i_NumObs, i_NumVRed, i_NumObsP2, i_NumVRedP2, da_DataRed, &
      da_DepVarValsRed, d_Toler, da_RedBetasLocal, da_RedResids,  &
      ia_Work, d_TempTheta, l_TrueLad)
da_RedBetas(1:i_NumVRed) = da_RedBetasLocal(1:i_NumVRed)
!----------------------- End calls to L1 routine --------------------- !
i_ExitCode = INT(da_DataRed(i_NumObsP2,i_NumVRedP1))
i_Iter     = INT(da_DataRed(i_NumObsP2,i_NumVRedP2))
d_SumAbsValRes = da_Data(i_NumObsP1,i_NumVarsP1)       ! for full model
d_SumAbsValResRed = da_DataRed(i_NumObsP1,i_NumVRedP1) ! same for red mod
     !----July 13 1998 new code to properly accumulate Weighted sums of the
! absolute deviations for *QUANTILE Regressions*==>(Quant is positive)
IF (.NOT.l_TrueLad) THEN ! Quantile Regression:
   d_WtSumAbsDevsRedMod = d_ZERO
   DO i=1,i_NumObs
      IF (da_RedResids(i) < d_ZERO) THEN ! if resid. is negative
         d_WtSumAbsDevsRedMod = d_WtSumAbsDevsRedMod + da_RedResids(i)*(d_TempTheta-d_ONE)
      ELSE                      ! if resid. is positive
         d_WtSumAbsDevsRedMod = d_WtSumAbsDevsRedMod + da_RedResids(i)*d_TempTheta
      END IF
   END DO
   d_WtSumAbsDevsFulMod = d_ZERO ! Full Model, Quantile Regression
   DO i=1,i_NumObs
      IF (da_Resids(i) < d_ZERO) THEN ! if resid. is negative
         d_WtSumAbsDevsFulMod = d_WtSumAbsDevsFulMod + da_Resids(i)*(d_TempTheta-d_ONE)
      ELSE                      ! if resid. is positive !
         d_WtSumAbsDevsFulMod = d_WtSumAbsDevsFulMod + da_Resids(i)*d_TempTheta
      END IF
   END DO
END IF
     !---- end July 13 1998 code segment -------------------------------- !
! if d_SumAbsValRes = 0 then we don't want to divide by it.  In fact if tF1
! sum of the absolute values of the residuals is zero, we have a
! perfect fit and the P-value is 1.0; we don't need to go through the
! contortions of the permutations.  We can just put out our P-value
! and put out "Undefined" as value of the test statistic d_To_LQ
! (aka Tee-zero).
IF (l_TrueLad) THEN
   IF (d_SumAbsValRes <= d_SMALL_VALUE) THEN
      l_HaveZero = .TRUE.  ! Don't divide by Zero... a
   ELSE       ! TestStat:
      d_To_LQ = (d_SumAbsValResRed-d_SumAbsValRes) / d_SumAbsValRes
   END IF
ELSE ! not a true LAD, a quantile regression
   IF (d_WtSumAbsDevsFulMod <= d_SMALL_VALUE) THEN
      l_HaveZero = .TRUE.  ! Don't divide by Zero... a
   ELSE       ! TestStat:
      d_To_LQ = (d_WtSumAbsDevsRedMod-d_WtSumAbsDevsFulMod) / d_WtSumAbsDevsFulMod
   END IF
END IF
IF (l_HaveZero) THEN
   d_PValue = d_ONE
   d_SumAbsValResRed = d_ZERO
   d_WtSumAbsDevsRedMod = d_ZERO
!  _______________________________________
! |                                       |
! | Perfect fit, zero residuals in model. |
! | We can't test this any further.       |
! |_______________________________________|
!
   CALL errhand(ch_MyMsg,ch_Msg=ch_REGRSMOD_EM020)
   iEr = i_NOT_OK
   GOTO 80000 !...  RETURN TO CALLING PROCEDURE, no need to test.
END IF
d_SumAbsValResRed1st = d_SumAbsValResRed
!---------------------------------------------------------------------
da_RedResids1st(1:i_NumObs) = da_RedResids(1:i_NumObs)
da_Data(1:i_NumObs,1:i_NumVarsP1) = da_DataSave(1:i_NumObs,1:i_NumVarsP1)
d_To = d_To_LQ
IF (ALLOCATED(da_DataRed      )) DEALLOCATE(da_DataRed      ,STAT=ios)
IF (ALLOCATED(da_DepVarValsRed)) DEALLOCATE(da_DepVarValsRed,STAT=ios)
IF (ALLOCATED(da_DepVarVals   )) DEALLOCATE(da_DepVarVals   ,STAT=ios)
IF (ALLOCATED(da_DataSave     )) DEALLOCATE(da_DataSave     ,STAT=ios)
IF (ALLOCATED(ia_Work         )) DEALLOCATE(ia_Work         ,STAT=ios)
ALLOCATE(da_DataQM1(1:i_NumObsQM1P2,1:i_NumVarsP2), &
         da_DataRed(1:i_NumObsQM1P2,1:i_NumVRedP2), &
       da_ResidsQM1(1:i_NumObsQM1), &
      da_DepVarVals(1:i_NumObsQM1), &
    da_RedResidsQM1(1:i_NumObsQM1), &
   da_DepVarValsRed(1:i_NumObsQM1), &
     da_ResRedDandP(1:i_NumObsQM1), &
        ia_DropZero(1:i_NumObs),    &
            ia_Work(1:i_NumObs),    &
               STAT=ios)
IF (ios /= i_OK) THEN
   CALL errhand(ch_MyMsg,i_Code=ios,            &
                ch_Msg=ch_EM00033,        &
                 ch_S1="dynamic arrays",  &
                 ch_S2="htqm1least")
   iEr = i_NOT_OK
   GOTO 80000
END IF
i_Cnt = 0
j = 1
DO i=1,i_NumObs
   IF (i_Cnt < i_Qm1  .AND.  da_RedResids1st(i) == d_ZERO) THEN
      ia_DropZero(i) = 1
      i_Cnt = i_Cnt + 1
   ELSE
      da_ResRedDandP(j) = da_RedResids1st(i)
      j = j + 1
      ia_DropZero(i) = 0
   END IF
END DO
! Now we need to fill the new data matrix with all but the DROPped
! observations.  We need all the matrices allocated to accomodate
! this new size.  Each time through the permutation loop, we need to
! pemute the full Reduced Residual vector and place it in the data
! array, dropping whichever observations (rows) that our dropzero
! values align with. Each permutation will result in some different
! row(s) being dropped.
i_Cnt2Perm = 1
CALL ipermut(ia_DropZero, i_NumObs) ! Permute the drop zero array
CALL permut(da_ResRedDandP, i_NumObsQM1) ! Permute the ResRedDandP array
i_Cnt = 0
DO i=1,i_NumObs ! Fill the data matrix da_DataQM1
   IF (ia_DropZero(i) == 0) THEN
      i_Cnt = i_Cnt + 1
      da_DataQM1(i_Cnt,1:i_NumVars) = da_Data(i,1:i_NumVars)
      da_DataQM1(i_Cnt,i_NumVarsP1) = da_ResRedDandP(i_Cnt)
   END IF
   call rchkusr()
END DO

IF (l_SAVETEST) THEN ! We want to save original and all resampled test statistics
   da_STV(1) = d_To
END IF
  ! vvvvvvvvvv   B E G I N   P E R M U T A T I O N   L O O P   vvvvvvvvvv
DO i=2,i_NumPermut
   !---------------------------------------------------------------------
   ! Fill data matrix for reduced model. Also, remember that
   ! the last column in data matrix is dependent variable.
   ! If we have a reduced model, we will drop some variables.

   i_JCount = 1
   DO j=1,i_NumVarsP1
      IF (j == i_NumVarsP1) THEN
         da_DataRed(1:i_NumObsQM1,i_JCount) = da_DataQM1(1:i_NumObsQM1,j) ! this is dependent var
      ELSE
         IF (ia_PosToDrop(j) == 0) THEN
            da_DataRed(1:i_NumObsQM1,i_JCount) = da_DataQM1(1:i_NumObsQM1,j) ! indep var values
            i_JCount = i_JCount + 1
         END IF
      END IF
      call rchkusr()
   END DO
   d_SumAbsValResRed    = d_ZERO
   da_DepVarVals(1:i_NumObsQM1) = da_DataQM1(1:i_NumObsQM1,i_NumVarsP1)   ! keep copies
   da_DepVarValsRed(1:i_NumObsQM1) = da_DataRed(1:i_NumObsQM1,i_NumVRedP1)
   !--------------------- Begin calls to L1 routine --------------------- !

   CALL L1(i_NumObsQM1, i_NumVars, i_NumObsQM1P2, i_NumVarsP2, da_DataQM1, &
         da_DepVarVals,    d_Toler, da_BetasLocal,    da_ResidsQM1,        &
         ia_Work, d_TempTheta, l_TrueLad)

   CALL L1(i_NumObsQM1, i_NumVRed, i_NumObsQM1P2, i_NumVRedP2, da_DataRed, &
         da_DepVarValsRed, d_Toler, da_RedBetasLocal, da_RedResidsQM1,     &
         ia_Work, d_TempTheta, l_TrueLad)
        !----------------------- End calls to L1 routine --------------------- !
   d_SumAbsValRes =    da_DataQM1(i_NumObsQM1P1,i_NumVarsP1)         ! for full model
   d_SumAbsValResRed = da_DataRed(i_NumObsQM1P1,i_NumVRedP1) ! same for red mod
   !----July 13 1998 new code to properly accumulate Weighted sums of the
   ! absolute deviations for *QUANTILE Regressions*==>(Quant is positive)
   IF (.NOT.l_TrueLad) THEN ! Quantile Regression:
      d_WtSumAbsDevsRedModLocal = d_ZERO
      DO i1=1,i_NumObsQM1
         IF (da_RedResidsQM1(i1) < d_ZERO) THEN ! if resid. is negative
            d_WtSumAbsDevsRedModLocal = d_WtSumAbsDevsRedModLocal + da_RedResidsQM1(i1)*(d_TempTheta-d_ONE)
         ELSE                      ! if resid. is positive
            d_WtSumAbsDevsRedModLocal = d_WtSumAbsDevsRedModLocal + da_RedResidsQM1(i1)*d_TempTheta
         END IF
         call rchkusr()
      END DO
      d_WtSumAbsDevsFulMod = d_ZERO ! Full Model, Quantile Regression
      DO i1=1,i_NumObsQM1
         IF (da_ResidsQM1(i1) < d_ZERO) THEN    ! if resid. is negative
            d_WtSumAbsDevsFulMod = d_WtSumAbsDevsFulMod + da_ResidsQM1(i1)*(d_TempTheta-d_ONE)
         ELSE                      ! if resid. is positive !
            d_WtSumAbsDevsFulMod = d_WtSumAbsDevsFulMod + da_ResidsQM1(i1)*d_TempTheta
         END IF
      END DO
   END IF ! (l_DoQuantRegression)
   !---- end July 13 1998 code segment -------------------------------- !
   IF (l_TrueLad) THEN
      IF (d_SumAbsValRes <= d_SMALL_VALUE) THEN ! Don't divide by Zero...
         d_To_LQ = d_LARGE_VALUE ! make it big enough to increment i_Cnt2Perm
      ELSE       ! TestStat:
         d_To_LQ = (d_SumAbsValResRed-d_SumAbsValRes) / d_SumAbsValRes
      END IF
   ELSE ! not a true LAD, a quantile regression
      IF (d_WtSumAbsDevsFulMod <= d_SMALL_VALUE) THEN ! Don't divide by Zero...
         d_To_LQ = d_LARGE_VALUE ! make it big enough to increment i_Cnt2Perm
      ELSE       ! TestStat:
         d_To_LQ = (d_WtSumAbsDevsRedModLocal-d_WtSumAbsDevsFulMod) / d_WtSumAbsDevsFulMod
      END IF
   END IF
   IF (l_SAVETEST) THEN    ! Save value of test stat for this permuted sample
      da_STV(i) = d_To_LQ
   END IF
   !---------------------------------------------------------------------
   IF (d_To_LQ >= d_To) THEN
      i_Cnt2Perm = i_Cnt2Perm + 1
   END IF
   CALL ipermut(ia_DropZero, i_NumObs) ! Permute the drop zero array
   CALL permut(da_ResRedDandP, i_NumObsQM1) ! Permute the ResRedDandP array
   i_Cnt = 0
   DO i1=1,i_NumObs           ! Fill the data matrix da_DataQM1
      IF (ia_DropZero(i1) == 0) THEN
         i_Cnt = i_Cnt + 1
         da_DataQM1(i_Cnt,1:i_NumVars) = da_Data(i1,1:i_NumVars)
         da_DataQM1(i_Cnt,i_NumVarsP1) = da_ResRedDandP(i_Cnt)
      END IF
      call rchkusr()
   END DO
END DO ! end of permutation loop

! /////////////////////////////////////////////////////////////////////////////////////////////
!
! Check to see that we have access to output file
!
! /////////////////////////////////////////////////////////////////////////////////////////////



  ! ^^^^^^^^^^^^^^   E N D   P E R M U T A T I O N   L O O P   ^^^^^^^^^^
! Things to report:
d_PValue = DBLE(i_Cnt2Perm)/DBLE(i_NumPermut)
d_SumAbsValResRed = d_SumAbsValResRed1st
80000 CONTINUE
! deallocate array space:
IF (ALLOCATED(ia_Work         )) DEALLOCATE(ia_Work,         STAT=ios)
IF (ALLOCATED(da_BetasLocal   )) DEALLOCATE(da_BetasLocal,   STAT=ios)
IF (ALLOCATED(ia_DropZero     )) DEALLOCATE(ia_DropZero,     STAT=ios)
IF (ALLOCATED(da_ResRedDandP  )) DEALLOCATE(da_ResRedDandP,  STAT=ios)
IF (ALLOCATED(da_DepVarValsRed)) DEALLOCATE(da_DepVarValsRed,STAT=ios)
IF (ALLOCATED(da_RedResidsQM1 )) DEALLOCATE(da_RedResidsQM1, STAT=ios)
IF (ALLOCATED(da_DepVarVals   )) DEALLOCATE(da_DepVarVals,   STAT=ios)
IF (ALLOCATED(da_ResidsQM1    )) DEALLOCATE(da_ResidsQM1,    STAT=ios)
IF (ALLOCATED(da_DataRed      )) DEALLOCATE(da_DataRed,      STAT=ios)
IF (ALLOCATED(da_DataQM1      )) DEALLOCATE(da_DataQM1,      STAT=ios)
IF (ALLOCATED(da_RedBetasLocal)) DEALLOCATE(da_RedBetasLocal,STAT=ios)
IF (ALLOCATED(da_RedResids1st )) DEALLOCATE(da_RedResids1st, STAT=ios)
RETURN
END SUBROUTINE htqm1least

!==============================================================================

SUBROUTINE htrsleast(i_NumObs, i_NumVars, d_Theta, i_NumPermut, &
                     i_RandNumSeed, i_NumToDrop, ia_PosToDrop, d_Toler, &
                     da_Data, &
                     d_To, d_Tn, d_PValue, d_PVTn, &
                     da_Betas, da_RedBetas, &
                     da_Resids, da_RedResids, &
                     d_SumAbsValResRed, d_WtSumAbsDevsRedMod, &
                     i_Iter, i_ExitCode, iEr,ch_MyMsg,da_STV,l_SaveTest)
!___Imported Parameters and Variables:

        ! ^-- Misc. Blossom file names, units, status parameters.
USE blcmnmod, ONLY: d_ONE, d_ZERO, i_NOT_OK, i_OK
        ! ^-- Some global Blossom variables and parameters.
USE jonsmodule, ONLY: i_CCode, i_REGSRMOD_ER007, ch_EM00033
        ! ^-- Message handling, etc.
!___Imported Procedures:
USE csgamma, ONLY: CS_Prob
        ! ^-- CS_Prob: chi-square probability estimate
USE invertmod, ONLY: invert
        ! ^__ invert: matrix inversion
USE jonsmodule, ONLY: errhand
        
USE mt19937, ONLY: init_genrand
        ! ^-- Mersenne Twister Pseudo Random Number Generator
IMPLICIT NONE
!___Dummy Arguments:
INTEGER,          INTENT(IN)    :: i_NumObs
INTEGER,          INTENT(IN)    :: i_NumVars
REAL (KIND(0D0)), INTENT(IN)    :: d_Theta
INTEGER,          INTENT(IN)    :: i_NumPermut
INTEGER,          INTENT(IN)    :: i_RandNumSeed
INTEGER,          INTENT(IN)    :: i_NumToDrop
INTEGER,          INTENT(IN)    :: ia_PosToDrop(:)
REAL (KIND(0D0)), INTENT(IN)    :: d_Toler
REAL (KIND(0D0)), INTENT(INOUT) :: da_Data(:,:)
REAL (KIND(0D0)), INTENT(OUT)   :: d_To
REAL (KIND(0D0)), INTENT(OUT)   :: d_Tn
REAL (KIND(0D0)), INTENT(OUT)   :: d_PValue
REAL (KIND(0D0)), INTENT(OUT)   :: d_PVTn
REAL (KIND(0D0)), INTENT(OUT)   :: da_Betas(:)
REAL (KIND(0D0)), INTENT(OUT)   :: da_RedBetas(:)
REAL (KIND(0D0)), INTENT(OUT)   :: da_Resids(:)
REAL (KIND(0D0)), INTENT(OUT)   :: da_RedResids(:)
REAL (KIND(0D0)), INTENT(OUT)   :: d_SumAbsValResRed
REAL (KIND(0D0)), INTENT(OUT)   :: d_WtSumAbsDevsRedMod
INTEGER,          INTENT(OUT)   :: i_Iter
INTEGER,          INTENT(OUT)   :: i_ExitCode
INTEGER,          INTENT(OUT)   :: iEr
INTEGER, PARAMETER :: i_LEN_MY_EMSG = 254 ! needs to be this big for output of var list in dostatus.
CHARACTER (LEN=i_LEN_MY_EMSG), INTENT(OUT) :: ch_MyMsg
REAL (KIND(0D0)), INTENT(OUT)  :: da_STV(:) ! save test values
LOGICAL,          INTENT(IN)     :: l_SaveTest
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
!     SUBROUTINE HTRSLEAST
! DESCRIPTION:
!     Perform hypothesis test with rank score testing of lad or quantile
!     regression (no double permutation). Here the transformed rank scores
!     are permuted in the permutation testing loop. Rerurn results of
!     reduced model and test.
!
! HYP / RANKSCORE .........................................(HYP_RANKSCORE)
!    Condition:  1. First Command was LAD / QUANT=t
!                2. HYP command with reduced model
!                3. HYP / RANKSCORE as second command
!                4. (Not Double Permutation)
!                5. (Later we may want the program to
!                    force simple LAD command to
!                    act like it was QUANT=0.5)
!
!       Action:  Permute transformed rankscore vector and use as
!                observed values in original design matrix.
!
!                LAD / QUANT ->
!                -> L1 -> Dual Solution -> Transform Rank Scores -> ...
!
!                ... ->  RS-LSQ -> T(obs) -> |Transformed Rank Scores| ->
!                         (1)                |      permut(TRS)      |
!                                                 \ (2...NPerms) /
!                                                  \    loop    /
!                                                   <- RS-LSQ <-
!
!       Output:  Betas,
!                Sum of absolute values of the residuals,
!                Weighted sum of the absolute deviations,
!                Observed Rank Score Test Statistic,
!                P-value of Rank Score Test,
!                Asymptotic Rank Score Statistic,
!                P-Value of Asymptotic RS Stat
!
! LANGUAGE:
!     Fortran 90/95
! AUTHOR:
!     Jon D. Richards, Operations Research Analyst    jon_richards@usgs.gov
!     Fort Collins Science Center                  http://www.fort.usgs.gov
!     U.S. Geological Survey  -  Biological Resources Division
! MODIFICATION HISTORY:
!   CODE HISTORY:
!     CREATED - JDR Jan 2004
!     MOD - JDR Apr 2004 - Removed calls to rslsqredcalc into this routine and
!           replaced with inline code, which is short enough to include &
!           eliminate unneeded overhead of surboutine call.
!     Mod - Added option to have SAVETEST of regression test statistics for
!           original and permuted regressions to a file - JDR  24 Mar 2005
!     Mod - Add option to use Mersenne Twister Pseudo Random Number Generator
!           Initialize both PRNGs at once. - JDR Apr 2005
!   COMMENT HISTORY:
!     CREATED - JDR Jan 2004!
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
!___Local Variables:
REAL (KIND(0D0)), ALLOCATABLE :: da_DataRed(:,:)
REAL (KIND(0D0)), ALLOCATABLE :: da_DataSave(:,:)
REAL (KIND(0D0)), ALLOCATABLE :: da_Data_RS(:,:)
REAL (KIND(0D0)), ALLOCATABLE :: da_B(:,:)
REAL (KIND(0D0)), ALLOCATABLE :: da_BRed(:,:)
REAL (KIND(0D0)), ALLOCATABLE :: da_DualSoln(:)
REAL (KIND(0D0)), ALLOCATABLE :: da_RankScore(:)
REAL (KIND(0D0)), ALLOCATABLE :: da_DepVarVals(:)
REAL (KIND(0D0)), ALLOCATABLE :: da_DepVarValsRed(:)
REAL (KIND(0D0)), ALLOCATABLE :: da_C(:)
REAL (KIND(0D0)), ALLOCATABLE :: da_CRed(:)
REAL (KIND(0D0)), ALLOCATABLE :: da_E(:)    ! resids
REAL (KIND(0D0)), ALLOCATABLE :: da_RedResids1st(:) ! Resids for red mod, 1st time thru, not permuted
REAL (KIND(0D0)), ALLOCATABLE :: da_X(:)    ! regression coeffs
REAL (KIND(0D0)), ALLOCATABLE :: da_XRed(:) ! RedModel regr coeffs
REAL (KIND(0D0)), ALLOCATABLE :: da_YRS(:)  ! dependent obs
REAL (KIND(0D0)), ALLOCATABLE :: da_YH(:)   ! dependent ests
REAL (KIND(0D0)) :: d_SSR, d_SSRRed, d_TempTheta, d_TestStat

INTEGER, ALLOCATABLE :: ia_Work(:)
INTEGER :: i, i_CntPerms, i_CntStat, i_JCount, i_NumObsP1, i_NumObsP2, &
      i_NumVarsP1, i_NumVarsP2, i_NumVRed, i_NumVRedP1, i_NumVRedP2, ios, &
      i_Sz, j
LOGICAL :: l_FirstTimeThru, l_TrueLad
!___Intrinsic Procedures:
INTRINSIC :: ALLOCATED, DBLE, DOT_PRODUCT, HUGE, INT, MATMUL, RANDOM_SEED, &
             TRANSPOSE
!___Executable Statements:
!d CALL MyDebug(">> into HTRSLEAST")                                   ! debug!

                                      ! debug!
iEr = i_OK ! no problem yet
i_Iter = 0
i_ExitCode = -HUGE(1) ! absurd value
d_To                 = -HUGE(d_ONE) ! absurd value
d_Tn                 = -HUGE(d_ONE) ! absurd value
d_PValue             = -HUGE(d_ONE) ! absurd value
d_PVTn               = -HUGE(d_ONE) ! absurd value
d_SumAbsValResRed    = -HUGE(d_ONE) ! absurd value
d_WtSumAbsDevsRedMod = -HUGE(d_ONE) ! absurd value
! Initialize default PRNG
CALL RANDOM_SEED(SIZE=i_Sz)
CALL RANDOM_SEED(PUT=(/(i_RandNumSeed,i=1,i_Sz)/))
! Initialize Mersenne Twister PRNG
CALL init_genrand(i_RandNumSeed)
i_NumVarsP1   = i_NumVars + 1
i_NumVarsP2   = i_NumVars + 2
i_NumObsP1    = i_NumObs  + 1
i_NumObsP2    = i_NumObs  + 2
i_NumVRed     = i_NumVars - i_NumToDrop
i_NumVRedP1   = i_NumVRed + 1
i_NumVRedP2   = i_NumVRed + 2
IF (d_Theta < d_ZERO) THEN
   l_TrueLad = .TRUE.
   d_TempTheta = -d_Theta
ELSE
   d_TempTheta = d_Theta
   l_TrueLad = .FALSE.
END IF
ALLOCATE(da_DualSoln(1:i_NumObs), &
        da_RankScore(1:i_NumObs), &
       da_DepVarVals(1:i_NumObs), &
    da_DepVarValsRed(1:i_NumObs), &
             ia_Work(1:i_NumObs), &
          da_DataRed(1:i_NumObsP2,1:i_NumVRedP2), &
         da_DataSave(1:i_NumObsP2,1:i_NumVarsP2), &
               STAT=ios)
IF (ios /= i_OK) THEN ! Memory allocation error
   CALL errhand(ch_MyMsg,i_Code=ios, &
                ch_Msg=ch_EM00033, &
                ch_S1="dynamic arrays", &
                ch_S2="htrsleast")
   iEr = i_NOT_OK
   GOTO 80000
END IF
!------------------------------------------------------------------------------
! Fill an da_Data data matrix for reduced model. Also, remember that
! the last column in da_Data matrix is dependent variable.
! If we have a reduced model, we will drop some variables.
i_JCount = 1
DO j=1,i_NumVarsP1
   IF (j == i_NumVarsP1) THEN
      da_DataRed(1:i_NumObs,i_JCount) = da_Data(1:i_NumObs,j) ! this is dependent var
   ELSE
      IF (ia_PosToDrop(j) == 0) THEN
         da_DataRed(1:i_NumObs,i_JCount) = da_Data(1:i_NumObs,j) ! indep var values
         i_JCount = i_JCount + 1
      END IF
   END IF
   call rchkusr()
END DO
d_WtSumAbsDevsRedMod = d_ZERO
d_SumAbsValResRed    = d_ZERO
da_DataSave(1:i_NumObs,1:i_NumVarsP1) = da_Data(1:i_NumObs,1:i_NumVarsP1) ! keep copy
da_DepVarVals(1:i_NumObs) = da_Data(1:i_NumObs,i_NumVarsP1)   ! keep copies of these
da_DepVarValsRed(1:i_NumObs) = da_DataRed(1:i_NumObs,i_NumVRedP1)
!--------------------- Begin calls to L1 routine --------------------- !
CALL L1(i_NumObs, i_NumVars, i_NumObsP2, i_NumVarsP2, da_Data,             &
      da_DepVarVals, d_Toler, da_Betas, da_Resids, ia_Work, d_TempTheta,   &
      l_TrueLad)
CALL L1(i_NumObs, i_NumVRed, i_NumObsP2, i_NumVRedP2, da_DataRed,    &
      da_DepVarValsRed, d_Toler, da_RedBetas, da_RedResids, ia_Work, &
      d_TempTheta, l_TrueLad, da_DualSoln)
!----------------------- End calls to L1 routine --------------------- !
i_ExitCode    = INT(da_DataRed(i_NumObsP2,i_NumVRedP1))
i_Iter        = INT(da_DataRed(i_NumObsP2,i_NumVRedP2))
d_SumAbsValResRed = da_DataRed(i_NumObsP1,i_NumVRedP1) ! same for red mod
!----July 13 1998 new code to properly accumulate Weighted sums of the
! absolute deviations for *QUANTILE Regressions*==>(Quant is positive)
d_WtSumAbsDevsRedMod = d_ZERO
IF (.NOT.l_TrueLad) THEN
   DO i=1,i_NumObs
      IF (da_RedResids(i) < d_ZERO) THEN ! if resid. is negative
         d_WtSumAbsDevsRedMod = d_WtSumAbsDevsRedMod + da_RedResids(i)*(d_TempTheta-d_ONE)
      ELSE                      ! if resid. is positive
         d_WtSumAbsDevsRedMod = d_WtSumAbsDevsRedMod + da_RedResids(i)*d_TempTheta
      END IF
      call rchkusr()
   END DO
END IF
!---- end July 13 1998 code segment -------------------------------- !
da_Data(1:i_NumObs,1:i_NumVarsP1) = da_DataSave(1:i_NumObs,1:i_NumVarsP1)
IF (ALLOCATED(da_DataSave     )) DEALLOCATE(da_DataSave,     STAT=ios)
IF (ALLOCATED(da_DataRed      )) DEALLOCATE(da_DataRed,      STAT=ios)
IF (ALLOCATED(ia_Work         )) DEALLOCATE(ia_Work,         STAT=ios)
IF (ALLOCATED(da_DepVarValsRed)) DEALLOCATE(da_DepVarValsRed,STAT=ios)
IF (ALLOCATED(da_DepVarVals   )) DEALLOCATE(da_DepVarVals,   STAT=ios)
!------------------------------------------------------------------------------
ALLOCATE(da_Data_RS(1:i_NumObs,1:i_NumVarsP1),  &
         da_DataRed(1:i_NumObs, 1:i_NumVRedP1), & ! reduced model data
               da_B(1:i_NumVars,1:i_NumVars),   &
            da_BRed(1:i_NumVRed,1:i_NumVRed),   &
    da_RedResids1st(1:i_NumObs),  & ! residuals, reduced model
               da_E(1:i_NumObs),  & ! residuals
              da_YH(1:i_NumObs),  & ! estimated values of dep. var.
             da_YRS(1:i_NumObs),  & ! observed values of dependent variable
               da_X(1:i_NumVars), & ! regression coefficients
               da_C(1:i_NumVars), &
            da_XRed(1:i_NumVRed), & ! regression coeffs, reduced model
            da_CRed(1:i_NumVRed), &
               STAT=ios)
IF (ios /= i_OK) THEN ! Memory allocation error
   CALL errhand(ch_MyMsg,i_Code=ios, &
                ch_Msg=ch_EM00033, &
                ch_S1="dynamic arrays 2", &
                ch_S2="htrsleast")
   iEr = i_NOT_OK
   GOTO 80000
END IF
da_RankScore(1:i_NumObs) = da_DualSoln(1:i_NumObs) - (d_ONE-ABS(d_TempTheta))

DO i=1,i_NumObs
   da_Data_RS(i,1:i_NumVars) = da_Data(i,1:i_NumVars)
   da_Data_RS(i,i_NumVarsP1) = da_RankScore(i)
END DO
!..............................................................................
! Fill a data matrix for reduced model. Also, remember that last
! column in A matrix is dependent variable.  If we have a reduced
! model, we will drop some variables.
i_JCount = 1
DO j=1,i_NumVarsP1
   IF (j == i_NumVarsP1) THEN
      da_DataRed(1:i_NumObs,i_JCount) = da_Data_RS(1:i_NumObs,j) ! The dependent variable
   ELSE
      IF (ia_PosToDrop(j) == 0) THEN
         da_DataRed(1:i_NumObs,i_JCount) = da_Data_RS(1:i_NumObs,j) ! Independent variables
         i_JCount = i_JCount + 1
      END IF
   END IF
END DO
i_CntStat  = 0       ! initialize counter for statistic, full model
i_CntPerms = 0       ! number of permutations that have run
l_FirstTimeThru = .TRUE.



! ------------------- main permutation loop (Rank Scores) ----------
MainPermLoop : DO
   IF (l_FirstTimeThru) THEN
      da_YRS(1:i_NumObs) = da_Data_RS(1:i_NumObs,i_NumVarsP1) ! (da_Data_RS not destoyed below)
   ELSE
      ! For the reduced model permute the residual vector and use this
      ! in the next regression in place of the observed dep. var values.
      CALL permut(da_RedResids1st, i_NumObs)
      da_YRS(1:i_NumObs) = da_RedResids1st(1:i_NumObs)
   END IF
            ! ----v----- BEGIN calculate resgression items --------v----- !
   ! If we are testing reduced model, we want to compare results of
   ! regression of full model (with permuted Y*) with regression of
   ! reduced model (with same Y*).
   ! CALL rslsqredcalc(i_NumVars, i_NumObs, da_Data_RS, da_YRS, da_C,    &
   !                   da_B,    da_X,    da_YH, da_E, d_SSR,    iEr)
   !

   da_C(1:i_NumVars) = MATMUL(da_YRS(1:i_NumObs), da_Data_RS(1:i_NumObs,1:i_NumVars))
   da_B(1:i_NumVars,1:i_NumVars) = MATMUL(TRANSPOSE(da_Data_RS(1:i_NumObs,1:i_NumVars)), &
                           da_Data_RS(1:i_NumObs,1:i_NumVars))
   ! invert da_B.
   CALL invert(i_NumVars, da_B(1:i_NumVars,1:i_NumVars), iEr)
   IF (iEr /= i_OK) THEN   ! in the case of a singular matrix, can't invert
      GOTO 80000   ! ... error exit ...
   END IF
   da_X(1:i_NumVars) = MATMUL(da_C(1:i_NumVars), da_B(1:i_NumVars,1:i_NumVars)) ! regression coefficient

   da_YH(1:i_NumObs) = MATMUL(da_X(1:i_NumVars), &
            TRANSPOSE(da_Data_RS(1:i_NumObs,1:i_NumVars)))  ! estimates of dep var

   da_E(1:i_NumObs) = da_YRS(1:i_NumObs) - da_YH(1:i_NumObs) ! array of residuals

   d_SSR = DOT_PRODUCT(da_E(1:i_NumObs), da_E(1:i_NumObs))

   da_CRed(1:i_NumVRed) = MATMUL(da_YRS(1:i_NumObs), da_DataRed(1:i_NumObs,1:i_NumVRed))
   da_BRed(1:i_NumVRed,1:i_NumVRed) = MATMUL(TRANSPOSE(da_DataRed(1:i_NumObs,1:i_NumVRed)), &
                              da_DataRed(1:i_NumObs,1:i_NumVRed))
           ! invert da_B.
   CALL invert(i_NumVRed, da_BRed(1:i_NumVRed,1:i_NumVRed), iEr)
   IF (iEr /= i_OK) THEN   ! in the case of a singular matrix, can't invert
      GOTO 80000   ! ... error exit ...
   END IF
   da_XRed(1:i_NumVRed) = MATMUL(da_CRed(1:i_NumVRed), da_BRed(1:i_NumVRed,1:i_NumVRed)) ! regression coefficient

   da_YH(1:i_NumObs) = MATMUL(da_XRed(1:i_NumVRed), &
            TRANSPOSE(da_DataRed(1:i_NumObs,1:i_NumVRed)))  ! estimates of dep var

   da_E(1:i_NumObs) = da_YRS(1:i_NumObs) - da_YH(1:i_NumObs) ! array of residuals

   d_SSRRed = DOT_PRODUCT(da_E(1:i_NumObs), da_E(1:i_NumObs))
   !
   !    IF (iEr /= i_OK ) RETURN   ! ... error exit ...
            ! ----^----- END calculate resgression items ----------^----- !
   IF (l_FirstTimeThru) THEN
        ! 1st time, regression itself satisfies condition TestStat>=TObserved
      i_CntStat = 1
      da_RedResids1st(1:i_NumObs) = da_E(1:i_NumObs)
      d_To = (d_SSRRed-d_SSR) / d_SSR ! Observed Test Statistic.  ! ? To*(1-prec) ?
      IF (l_SAVETEST) THEN  ! Save value of test stat for this permuted sample
         da_STV(1) = d_To
      END IF
      d_Tn = (d_SSRRed-d_SSR) / (d_TempTheta*(d_ONE - d_TempTheta)) ! Asympt. RS Stat.

   END IF
   i_CntPerms = i_CntPerms + 1
   IF (l_FirstTimeThru) THEN
      l_FirstTimeThru = .FALSE.
   ELSE
      IF (i_CntPerms <= i_NumPermut) THEN ! For each permutation up to i_NumPermut:
         d_TestStat = (d_SSRRed-d_SSR) / d_SSR  ! The Permuted Test Statistic
         IF (l_SAVETEST) THEN  ! Save value of test stat for this permuted sample
            da_STV(i_CntPerms) = d_TestStat
         END IF
         IF (d_TestStat >= d_To) THEN ! Is Permuted Test Stat >= Obs Test Stat? ! ? > ?
            i_CntStat = i_CntStat + 1  ! ... if so, increment counter.
         END IF
      END IF
   END IF
   IF (i_CntPerms == i_NumPermut) EXIT MainPermLoop ! Done on last permutation.
   call rchkusr()
END DO MainPermLoop

! /////////////////////////////////////////////////////////////////////////////////////////////
!
! Check to see that we have access to output file
!
! /////////////////////////////////////////////////////////////////////////////////////////////

        ! ------------------ END main permutation loop (Rank Scores) --------
d_PValue = DBLE(i_CntStat)/DBLE(i_NumPermut) ! P-Value is this simple proportion.
d_PVTn = CS_Prob(i_NumToDrop, d_Tn) ! P-value of asymptotic RS Test Stat      <<<<<<<<<<<<<<<<< REPORT THIS

!..............................................................................
80000 CONTINUE
IF (ALLOCATED(da_CRed        )) DEALLOCATE(da_CRed        ,STAT=ios)
IF (ALLOCATED(da_XRed        )) DEALLOCATE(da_XRed        ,STAT=ios)
IF (ALLOCATED(da_C           )) DEALLOCATE(da_C           ,STAT=ios)
IF (ALLOCATED(da_X           )) DEALLOCATE(da_X           ,STAT=ios)
IF (ALLOCATED(da_YRS         )) DEALLOCATE(da_YRS         ,STAT=ios)
IF (ALLOCATED(da_YH          )) DEALLOCATE(da_YH          ,STAT=ios)
IF (ALLOCATED(da_E           )) DEALLOCATE(da_E           ,STAT=ios)
IF (ALLOCATED(da_RedResids1st)) DEALLOCATE(da_RedResids1st,STAT=ios)
IF (ALLOCATED(da_BRed        )) DEALLOCATE(da_BRed        ,STAT=ios)
IF (ALLOCATED(da_B           )) DEALLOCATE(da_B           ,STAT=ios)
IF (ALLOCATED(da_DataRed     )) DEALLOCATE(da_DataRed     ,STAT=ios)
IF (ALLOCATED(da_Data_RS     )) DEALLOCATE(da_Data_RS     ,STAT=ios)
IF (ALLOCATED(da_RankScore   )) DEALLOCATE(da_RankScore   ,STAT=ios)
IF (ALLOCATED(da_DualSoln    )) DEALLOCATE(da_DualSoln    ,STAT=ios)

RETURN
END SUBROUTINE htrsleast

!==============================================================================

SUBROUTINE htrsdpleast(i_NumObs, i_NumVars, d_Theta, i_NumPermut, &
                       i_RandNumSeed, i_NumToDrop, ia_PosToDrop, d_Toler, &
                       da_Data, &
                       d_To, d_Tn, d_PValue, d_PVTn, &
                       da_Betas, da_RedBetas, &
                       da_Resids, da_RedResids, &
                       d_SumAbsValResRed, d_WtSumAbsDevsRedMod, &
                       i_Iter, i_ExitCode, iEr,ch_MyMsg,da_STV,l_SaveTest)
!___Imported Parameters and Variables:

USE blcmnmod, ONLY: d_ONE, d_ZERO, i_NOT_OK, i_OK
        ! ^-- Some global Blossom variables and parameters.
USE jonsmodule, ONLY: i_CCode, i_REGSRMOD_ER007, ch_EM00033
        ! ^-- Message handling, etc.
!___Imported Procedures:
USE binomod, ONLY: random_binomial
        ! ^-- random_binomial: random binomial deviate
USE csgamma, ONLY: CS_Prob
        ! ^-- CS_Prob: chi-square probability estimate
USE invertmod, ONLY: invert
        ! ^__ invert: matrix inversion
USE jonsmodule, ONLY: errhand
        
USE mt19937, ONLY: init_genrand
        ! ^-- Mersenne Twister Pseudo Random Number Generator
IMPLICIT NONE
!___Dummy Arguments:
INTEGER,          INTENT(IN)    :: i_NumObs
INTEGER,          INTENT(IN)    :: i_NumVars
REAL (KIND(0D0)), INTENT(IN)    :: d_Theta
INTEGER,          INTENT(IN)    :: i_NumPermut
INTEGER,          INTENT(IN)    :: i_RandNumSeed
INTEGER,          INTENT(IN)    :: i_NumToDrop
INTEGER,          INTENT(IN)    :: ia_PosToDrop(:)
REAL (KIND(0D0)), INTENT(IN)    :: d_Toler
REAL (KIND(0D0)), INTENT(INOUT) :: da_Data(:,:)
REAL (KIND(0D0)), INTENT(OUT)   :: d_To
REAL (KIND(0D0)), INTENT(OUT)   :: d_Tn
REAL (KIND(0D0)), INTENT(OUT)   :: d_PValue
REAL (KIND(0D0)), INTENT(OUT)   :: d_PVTn
REAL (KIND(0D0)), INTENT(OUT)   :: da_Betas(:)
REAL (KIND(0D0)), INTENT(OUT)   :: da_RedBetas(:)
REAL (KIND(0D0)), INTENT(OUT)   :: da_Resids(:)
REAL (KIND(0D0)), INTENT(OUT)   :: da_RedResids(:)
REAL (KIND(0D0)), INTENT(OUT)   :: d_SumAbsValResRed
REAL (KIND(0D0)), INTENT(OUT)   :: d_WtSumAbsDevsRedMod
INTEGER,          INTENT(OUT)   :: i_Iter
INTEGER,          INTENT(OUT)   :: i_ExitCode
INTEGER,          INTENT(OUT)   :: iEr
INTEGER, PARAMETER :: i_LEN_MY_EMSG = 254 ! needs to be this big for output of var list in dostatus.
CHARACTER (LEN=i_LEN_MY_EMSG), INTENT(OUT) :: ch_MyMsg
REAL (KIND(0D0)), INTENT(OUT) :: da_STV(:) ! save test values
LOGICAL,          INTENT(IN)     :: l_SaveTest
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
!     SUBROUTINE HTRSDPLEAST
! DESCRIPTION:
!     Perform hypothesis test of reduced model of lad or quantile regression
!     with both rank score and double permutation options.  Return results of
!     reduced model and test.
!
! HYP / RANKSCORE DP ..........................(HYP_RANKSCORE_DOUBLE_PERM)
!    Condition:  1. First Command was LAD / QUANT=t
!                2. HYP command with reduced model
!                3. HYP / RANKSCORE DP
!                4. (Double Permutation)
!
!       Action:  LAD / QUANT ->
!                -> L1 -> Dual Solution -> RS-LSQ -> T(obs) -> ...
!                                            (1)
!
!                ... -> | Center                | ->
!                       | Compute RS (transform)|
!                       |      permut(TRS)      |
!                            \ (2...NPerms) /
!                             \    loop    /
!                              <- RS-LSQ <-
!
! LANGUAGE:
!     Fortran 90/95
! AUTHOR:
!     Jon D. Richards, Operations Research Analyst    jon_richards@usgs.gov
!     Fort Collins Science Center                  http://www.fort.usgs.gov
!     U.S. Geological Survey  -  Biological Resources Division
! MODIFICATION HISTORY:
!   CODE HISTORY:
!     CREATED - JDR Jan 2004
!     MOD - JDR Apr 2004 - Removed calls to rslsqredcalc into this routine and
!           replaced with inline code, which is short enough to include &
!           eliminate unneeded overhead of surboutine call.
!     Mod - Added option to have SAVETEST of regression test statistics for
!           original and permuted regressions to a file - JDR  24 Mar 2005
!     Mod - Add option to use Mersenne Twister Pseudo Random Number Generator
!           Initialize both PRNGs at once. - JDR Apr 2005
!   COMMENT HISTORY:
!     CREATED - JDR Jan 2004!
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
!___Local Variables:
REAL (KIND(0D0)), ALLOCATABLE :: da_DataRed(:,:)
REAL (KIND(0D0)), ALLOCATABLE :: da_DataSave(:,:)
REAL (KIND(0D0)), ALLOCATABLE :: da_Data_RS(:,:)
REAL (KIND(0D0)), ALLOCATABLE :: da_B(:,:)
REAL (KIND(0D0)), ALLOCATABLE :: da_BRed(:,:)
REAL (KIND(0D0)), ALLOCATABLE :: da_DualSoln(:)
REAL (KIND(0D0)), ALLOCATABLE :: da_RankScore(:)
REAL (KIND(0D0)), ALLOCATABLE :: da_DualSoln1st(:)
REAL (KIND(0D0)), ALLOCATABLE :: da_RankScore1st(:)
REAL (KIND(0D0)), ALLOCATABLE :: da_DepVarVals(:)
REAL (KIND(0D0)), ALLOCATABLE :: da_DepVarValsRed(:)
REAL (KIND(0D0)), ALLOCATABLE :: da_YRS(:)
REAL (KIND(0D0)), ALLOCATABLE :: da_YH(:)
REAL (KIND(0D0)), ALLOCATABLE :: da_E(:)
REAL (KIND(0D0)), ALLOCATABLE :: da_C(:)
REAL (KIND(0D0)), ALLOCATABLE :: da_CRed(:)
REAL (KIND(0D0)), ALLOCATABLE :: da_X(:)
REAL (KIND(0D0)), ALLOCATABLE :: da_XRed(:)
REAL (KIND(0D0)) :: d_SSR, d_SSRRed, d_TempTheta, d_To_Star

INTEGER, ALLOCATABLE :: ia_Work(:)
INTEGER :: i, i_Cnt2Perm, i_JCount, i_Num0to1, i_Num1to0, i_NumObsP1, &
      i_NumObsP2, i_NumVarsP1, i_NumVarsP2, i_NumVRed, i_NumVRedP1, &
      i_NumVRedP2, ios, i_Sz, i_Z0, i_ZE_Star, i_ZI, j, jj
LOGICAL :: l_CentRS, l_First, l_TrueLad
!___Intrinsic Procedures:
INTRINSIC :: ABS, ALLOCATED, COUNT, DBLE, DOT_PRODUCT, HUGE, INT, MATMUL, &
             MIN, RANDOM_SEED, REAL, TRANSPOSE
!___Executable Statements:

iEr = i_OK ! no problem yet
i_Iter = 0
i_ExitCode = -HUGE(1) ! absurd value
d_To                 = -HUGE(d_ONE) ! absurd value
d_Tn                 = -HUGE(d_ONE) ! absurd value
d_PValue             = -HUGE(d_ONE) ! absurd value
d_PVTn               = -HUGE(d_ONE) ! absurd value
d_SumAbsValResRed    = -HUGE(d_ONE) ! absurd value
d_WtSumAbsDevsRedMod = -HUGE(d_ONE) ! absurd value

! Initialize default PRNG
CALL RANDOM_SEED(SIZE=i_Sz)
CALL RANDOM_SEED(PUT=(/(i_RandNumSeed,i=1,i_Sz)/))
! Initialize Mersenne Twister PRNG
CALL init_genrand(i_RandNumSeed)
IF (d_Theta < d_ZERO) THEN
   l_TrueLad = .TRUE.
   d_TempTheta = -d_Theta
ELSE
   d_TempTheta = d_Theta
   l_TrueLad = .FALSE.
END IF
i_NumVarsP1   = i_NumVars + 1
i_NumVarsP2   = i_NumVars + 2
i_NumObsP1    = i_NumObs  + 1
i_NumObsP2    = i_NumObs  + 2
i_NumVRed     = i_NumVars - i_NumToDrop
i_NumVRedP1   = i_NumVRed + 1
i_NumVRedP2   = i_NumVRed + 2
ALLOCATE(da_DualSoln(1:i_NumObs), &
        da_RankScore(1:i_NumObs), &
      da_DualSoln1st(1:i_NumObs), &
     da_RankScore1st(1:i_NumObs), &
       da_DepVarVals(1:i_NumObs), &
    da_DepVarValsRed(1:i_NumObs), &
             ia_Work(1:i_NumObs), &
          da_DataRed(1:i_NumObsP2,1:i_NumVRedP2), &
         da_DataSave(1:i_NumObsP2,1:i_NumVarsP2), &
                  STAT=ios)
IF (ios /= i_OK) THEN ! Memory allocation error in LAD
   CALL errhand(ch_MyMsg,i_Code=ios, &
                ch_Msg=ch_EM00033, &
                ch_S1="dynamic arrays", &
                ch_S2="htrsdpleast")
   iEr = i_NOT_OK
   GOTO 80000
END IF
!------------------------------------------------------------------------------
iEr = i_OK ! no errors yet
! Fill an da_Data data matrix for reduced model. Also, remember that
! the last column in da_Data matrix is dependent variable.
! If we have a reduced model, we will drop some variables.
i_JCount = 1
DO j=1,i_NumVarsP1
   IF (j == i_NumVarsP1) THEN
      da_DataRed(1:i_NumObs,i_JCount) = da_Data(1:i_NumObs,j) ! this is dependent var
   ELSE
      IF (ia_PosToDrop(j) == 0) THEN
         da_DataRed(1:i_NumObs,i_JCount) = da_Data(1:i_NumObs,j) ! indep var values
         i_JCount = i_JCount + 1
      END IF
   END IF
   call rchkusr()
END DO
d_WtSumAbsDevsRedMod = d_ZERO
d_SumAbsValResRed    = d_ZERO
da_DataSave(1:i_NumObs,1:i_NumVarsP1) = da_Data(1:i_NumObs,1:i_NumVarsP1) ! keep copy
da_DepVarVals(1:i_NumObs) = da_Data(1:i_NumObs,i_NumVarsP1)
da_DepVarValsRed(1:i_NumObs) = da_DataRed(1:i_NumObs,i_NumVRedP1)
!--------------------- Begin calls to L1 routine --------------------- !
CALL L1(i_NumObs, i_NumVars, i_NumObsP2, i_NumVarsP2, da_Data,    &
        da_DepVarVals,    d_Toler, da_Betas,    da_Resids,    ia_Work, &
        d_TempTheta, l_TrueLad)
CALL L1(i_NumObs, i_NumVRed, i_NumObsP2, i_NumVRedP2, da_DataRed, &
        da_DepVarValsRed, d_Toler, da_RedBetas, da_RedResids, ia_Work, &
        d_TempTheta, l_TrueLad, da_DualSoln) !<<< The Dual Soln is what we want.
!----------------------- End calls to L1 routine --------------------- !
i_ExitCode    = INT(da_DataRed(i_NumObsP2,i_NumVRedP1)) !                     <<<<<<<<<<<<<<<<< REPORT THIS
i_Iter        = INT(da_DataRed(i_NumObsP2,i_NumVRedP2)) !                     <<<<<<<<<<<<<<<<< REPORT THIS
d_SumAbsValResRed = da_DataRed(i_NumObsP1,i_NumVRedP1) ! same for red mod     <<<<<<<<<<<<<<<<< REPORT THIS
!----July 13 1998 new code to properly accumulate Weighted sums of the
! absolute deviations for *QUANTILE Regressions*==>(Quant is positive)
d_WtSumAbsDevsRedMod = d_ZERO !                                               <<<<<<<<<<<<<<<<< REPORT THIS
IF (.NOT.l_TrueLad) THEN ! Quantile Regression:
   DO i=1,i_NumObs ! Reduced Model, Quantile Regression
      IF (da_RedResids(i) < d_ZERO) THEN ! if resid. is negative
         d_WtSumAbsDevsRedMod = d_WtSumAbsDevsRedMod + da_RedResids(i)*(d_TempTheta-d_ONE)
      ELSE                      ! if resid. is positive
         d_WtSumAbsDevsRedMod = d_WtSumAbsDevsRedMod + da_RedResids(i)*d_TempTheta
      END IF
      call rchkusr()
   END DO
END IF ! (.NOT.l_TrueLad) i.e., Quantile Regression
!---- end July 13 1998 code segment -------------------------------- !
da_Data(1:i_NumObs,1:i_NumVarsP1) = da_DataSave(1:i_NumObs,1:i_NumVarsP1)
IF (ALLOCATED(da_DataSave     )) DEALLOCATE(da_DataSave,     STAT=ios)
IF (ALLOCATED(da_DataRed      )) DEALLOCATE(da_DataRed,      STAT=ios)
IF (ALLOCATED(ia_Work         )) DEALLOCATE(ia_Work,         STAT=ios)
IF (ALLOCATED(da_DepVarValsRed)) DEALLOCATE(da_DepVarValsRed,STAT=ios)
IF (ALLOCATED(da_DepVarVals   )) DEALLOCATE(da_DepVarVals,   STAT=ios)
!------------------------------------------------------------------------------
ALLOCATE(da_Data_RS(1:i_NumObs, 1:i_NumVarsP1), &
         da_DataRed(1:i_NumObs, 1:i_NumVRedP1), & ! reduced model data
               da_B(1:i_NumVars,1:i_NumVars),   &
            da_BRed(1:i_NumVRed,1:i_NumVRed),   &
               da_E(1:i_NumObs),  & ! residuals
              da_YH(1:i_NumObs),  & ! estimated values of dep. var.
             da_YRS(1:i_NumObs),  & ! observed values of dependent variable
               da_X(1:i_NumVars), & ! regression coefficients
               da_C(1:i_NumVars), &
            da_XRed(1:i_NumVRed), & ! regression coeffs, reduced model
            da_CRed(1:i_NumVRed), &
               STAT=ios)
IF (ios /= i_OK) THEN ! Memory allocation error in LAD
   CALL errhand(ch_MyMsg,i_Code=ios, &
                ch_Msg=ch_EM00033, &
                ch_S1="dynamic arrays 2", &
                ch_S2="htrsdpleast")
   iEr = i_NOT_OK
   GOTO 80000
END IF
da_DualSoln1st(1:i_NumObs) = da_DualSoln(1:i_NumObs)
i_Z0 = COUNT(da_DualSoln1st(1:i_NumObs)==d_ZERO)
i_ZI = COUNT((da_DualSoln1st(1:i_NumObs)<d_ONE).AND.(da_DualSoln1st>d_ZERO))
da_RankScore1st(1:i_NumObs) = da_DualSoln1st(1:i_NumObs) - (d_ONE-d_TempTheta)
DO i=1,i_NumObs
   da_Data_RS(i,1:i_NumVars) = da_Data(i,1:i_NumVars)
   da_Data_RS(i,i_NumVarsP1) = da_RankScore1st(i)
END DO
! ............................................................................
! Fill a data matrix for reduced model. Also, remember that last
! column in A matrix is dependent variable.  If we have a reduced
! model, we will drop some variables.
i_JCount = 1
DO j=1,i_NumVarsP1
   IF (j == i_NumVarsP1) THEN
      da_DataRed(1:i_NumObs,i_JCount) = da_Data_RS(1:i_NumObs,j) ! The dependent variable
   ELSE
      IF (ia_PosToDrop(j) == 0) THEN
         da_DataRed(1:i_NumObs,i_JCount) = da_Data_RS(1:i_NumObs,j) ! Independent variables
         i_JCount = i_JCount + 1
      END IF
   END IF
   call rchkusr()
END DO
da_YRS(1:i_NumObs) = da_Data_RS(1:i_NumObs,i_NumVarsP1) ! (da_Data_RS not destoyed below)
         ! ----v----- BEGIN calculate resgression items --------v----- !
   ! CALL rslsqredcalc(i_NumVars, i_NumObs, da_Data_RS, da_YRS, da_C,    &
   !                   da_B,    da_X,    da_YH, da_E, d_SSR,    iEr)
   !
da_C(1:i_NumVars) = MATMUL(da_YRS(1:i_NumObs), da_Data_RS(1:i_NumObs,1:i_NumVars))
da_B(1:i_NumVars,1:i_NumVars) = MATMUL(TRANSPOSE(da_Data_RS(1:i_NumObs,1:i_NumVars)), &
                        da_Data_RS(1:i_NumObs,1:i_NumVars))
        ! invert da_B.
CALL invert(i_NumVars, da_B(1:i_NumVars,1:i_NumVars), iEr)
IF (iEr /= i_OK) THEN   ! in the case of a singular matrix, can't invert
   GOTO 80000   ! ... error exit ...
END IF
da_X(1:i_NumVars) = MATMUL(da_C(1:i_NumVars), da_B(1:i_NumVars,1:i_NumVars)) ! regression coefficient
da_YH(1:i_NumObs) = MATMUL(da_X(1:i_NumVars), &
         TRANSPOSE(da_Data_RS(1:i_NumObs,1:i_NumVars))) ! estimates of dep var
da_E(1:i_NumObs) = da_YRS(1:i_NumObs) - da_YH(1:i_NumObs) ! array of residuals
d_SSR = DOT_PRODUCT(da_E(1:i_NumObs), da_E(1:i_NumObs))

da_CRed(1:i_NumVRed) = MATMUL(da_YRS(1:i_NumObs), da_DataRed(1:i_NumObs,1:i_NumVRed))
da_BRed(1:i_NumVRed,1:i_NumVRed) = MATMUL(TRANSPOSE(da_DataRed(1:i_NumObs,1:i_NumVRed)), &
                           da_DataRed(1:i_NumObs,1:i_NumVRed))
        ! invert da_B.
CALL invert(i_NumVRed, da_BRed(1:i_NumVRed,1:i_NumVRed), iEr)
IF (iEr /= i_OK) THEN   ! in the case of a singular matrix, can't invert
   GOTO 80000  ! ... error exit ...
END IF
da_XRed(1:i_NumVRed) = MATMUL(da_CRed(1:i_NumVRed), da_BRed(1:i_NumVRed,1:i_NumVRed)) ! regression coefficient
da_YH(1:i_NumObs) = MATMUL(da_XRed(1:i_NumVRed), TRANSPOSE(da_DataRed(1:i_NumObs,1:i_NumVRed))) ! estimates of dep var
da_E(1:i_NumObs) = da_YRS(1:i_NumObs) - da_YH(1:i_NumObs) ! array of residuals
d_SSRRed = DOT_PRODUCT(da_E(1:i_NumObs), da_E(1:i_NumObs))
!
! IF (iEr /= i_OK ) RETURN   ! ... error exit ...
         ! ----^----- END calculate resgression items ----------^----- !
d_To = (d_SSRRed-d_SSR) / d_SSR ! Observed Rank Score Test Statistic:         <<<<<<<<<<<<<<<<< REPORT THIS
d_Tn = (d_SSRRed-d_SSR) / (d_TempTheta*(d_ONE - d_TempTheta)) ! Asymptotic RS Stat<<<<<<<<<<<<<<<<< REPORT THIS
IF (l_SAVETEST) THEN ! We want to save original and all resampled test statistics
   da_STV(1) = d_To
END IF

! ............................................................................
i_Cnt2Perm = 1
l_First = .TRUE.
! vvvvvvvvvv   B E G I N   P E R M U T A T I O N   L O O P   vvvvvvvvvv
DO j=2,i_NumPermut
   i_ZE_Star = random_binomial(i_NumObs, REAL(ABS(d_TempTheta)), l_First)
   l_First = .FALSE.
   l_CentRS = .TRUE.
   da_DualSoln(1:i_NumObs) = da_DualSoln1st(1:i_NumObs)
   IF (i_Z0+i_ZI == i_ZE_Star) THEN
      l_CentRS = .FALSE.
   ELSE IF (i_Z0+i_ZI < i_ZE_Star) THEN
      i_Num1to0 = i_ZE_Star - i_Z0 - i_ZI
      CALL chngxon(da_DualSoln, i_NumObs, i_Num1to0, d_ONE, d_ZERO)
   ELSE IF (i_Z0 + i_ZI > i_ZE_Star) THEN
      i_Num0to1 = MIN(i_Z0, i_Z0+i_ZI-i_ZE_Star)
      IF (i_Num0to1 > 0) CALL chngxon(da_DualSoln, i_NumObs, i_Num0to1, d_ZERO, d_ONE)
   END IF
   IF (l_CentRS) THEN
      da_RankScore(1:i_NumObs) = da_DualSoln(1:i_NumObs) - (d_ONE-ABS(d_TempTheta))
   ELSE
      da_RankScore(1:i_NumObs) = da_RankScore1st(1:i_NumObs)
   END IF
   CALL permut(da_RankScore, i_NumObs) ! Permute in any case.
   DO i=1,i_NumObs  ! reload data matrix with original data
                    ! and centered rank scores.
      da_Data_RS(i,1:i_NumVars) = da_Data(i,1:i_NumVars)
      da_Data_RS(i,i_NumVarsP1) = da_RankScore(i)
      call rchkusr()
   END DO
   ! ............................................................................
   i_JCount = 1
   DO jj=1,i_NumVarsP1
      IF (jj == i_NumVarsP1) THEN
         da_DataRed(1:i_NumObs,i_JCount) = da_Data_RS(1:i_NumObs,jj) ! The dep var
      ELSE
         IF (ia_PosToDrop(jj) == 0) THEN
            da_DataRed(1:i_NumObs,i_JCount) = da_Data_RS(1:i_NumObs,jj) ! Indep vars
            i_JCount = i_JCount + 1
         END IF
      END IF
   END DO
   da_YRS(1:i_NumObs) = da_Data_RS(1:i_NumObs,i_NumVarsP1) ! (da_Data_RS not destoyed below)
            ! ----v----- BEGIN calculate resgression items --------v----- !
   !
   da_C(1:i_NumVars) = MATMUL(da_YRS(1:i_NumObs), da_Data_RS(1:i_NumObs,1:i_NumVars))
   da_B(1:i_NumVars,1:i_NumVars) = MATMUL(TRANSPOSE(da_Data_RS(1:i_NumObs,1:i_NumVars)), &
                           da_Data_RS(1:i_NumObs,1:i_NumVars))
   CALL invert(i_NumVars, da_B(1:i_Numvars,1:i_NumVars), iEr)
   IF (iEr /= i_OK) THEN   ! in the case of a singular matrix, can't invert
      GOTO 80000   ! ... error exit ...
   END IF
   da_X(1:i_Numvars) = MATMUL(da_C(1:i_NumVars), da_B(1:i_NumVars,1:i_NumVars)) ! regression coefficient
   da_YH(1:i_NumObs) = MATMUL(da_X(1:i_Numvars), &
            TRANSPOSE(da_Data_RS(1:i_NumObs,1:i_NumVars))) ! estimates of dep var
   da_E(1:i_NumObs) = da_YRS(1:i_NumObs) - da_YH(1:i_NumObs) ! array of residuals
   d_SSR = DOT_PRODUCT(da_E(1:i_NumObs), da_E(1:i_NumObs))

   da_CRed(1:i_NumVRed) = MATMUL(da_YRS(1:i_NumObs), da_DataRed(1:i_NumObs,1:i_NumVRed))
   da_BRed(1:i_NumVRed,1:i_NumVRed) = MATMUL(TRANSPOSE(da_DataRed(1:i_NumObs,1:i_NumVRed)), &
                              da_DataRed(1:i_NumObs,1:i_NumVRed))
   CALL invert(i_NumVRed, da_BRed(1:i_NumVRed,1:i_NumVRed), iEr)
   IF (iEr /= i_OK) THEN   ! in the case of a singular matrix, can't invert
      GOTO 80000   ! ... error exit ...
   END IF
   da_XRed(1:i_NumVRed) = MATMUL(da_CRed(1:i_NumVRed), da_BRed(1:i_NumVRed,1:i_NumVRed)) ! regression coefficient
   da_YH(1:i_NumObs) = MATMUL(da_XRed(1:i_NumVRed), &
            TRANSPOSE(da_DataRed(1:i_NumObs,1:i_NumVRed))) ! estimates of dep var
   da_E(1:i_NumObs) = da_YRS(1:i_NumObs) - da_YH(1:i_NumObs) ! array of residuals
   d_SSRRed = DOT_PRODUCT(da_E(1:i_NumObs), da_E(1:i_NumObs))
   !
   ! IF (iEr /= i_OK ) RETURN   ! ... error exit ...
            ! ----^----- END calculate resgression items ----------^----- !
   d_To_Star = (d_SSRRed-d_SSR) / d_SSR ! * Observed * Test * Statistic * Permuted Case *
   IF (l_SAVETEST) THEN   ! Save value of test stat for this permuted sample
      da_STV(j) = d_To_Star
   END IF
   ! ............................................................................
   IF (d_To_Star >= d_To) THEN
      i_Cnt2Perm = i_Cnt2Perm + 1 ! This is what counts!
   END IF
   call rchkusr()
END DO

! /////////////////////////////////////////////////////////////////////////////////////////////
!
! Check to see that we have access to output file
!
! /////////////////////////////////////////////////////////////////////////////////////////////



! ^^^^^^^^^^^^^^   E N D   P E R M U T A T I O N   L O O P   ^^^^^^^^^^
d_PValue = DBLE(i_Cnt2Perm)/(DBLE(i_NumPermut)) ! P-value of RS Test Stat     <<<<<<<<<<<<<<<<< REPORT THIS
d_PVTn = CS_Prob(i_NumToDrop, d_Tn) ! P-value of asymptotic RS Test Stat      <<<<<<<<<<<<<<<<< REPORT THIS

80000 CONTINUE
IF (ALLOCATED(da_CRed        )) DEALLOCATE(da_CRed        ,STAT=ios)
IF (ALLOCATED(da_XRed        )) DEALLOCATE(da_XRed        ,STAT=ios)
IF (ALLOCATED(da_C           )) DEALLOCATE(da_C           ,STAT=ios)
IF (ALLOCATED(da_X           )) DEALLOCATE(da_X           ,STAT=ios)
IF (ALLOCATED(da_YRS         )) DEALLOCATE(da_YRS         ,STAT=ios)
IF (ALLOCATED(da_YH          )) DEALLOCATE(da_YH          ,STAT=ios)
IF (ALLOCATED(da_E           )) DEALLOCATE(da_E           ,STAT=ios)
IF (ALLOCATED(da_BRed        )) DEALLOCATE(da_BRed        ,STAT=ios)
IF (ALLOCATED(da_B           )) DEALLOCATE(da_B           ,STAT=ios)
IF (ALLOCATED(da_DataRed     )) DEALLOCATE(da_DataRed     ,STAT=ios)
IF (ALLOCATED(da_Data_RS     )) DEALLOCATE(da_Data_RS     ,STAT=ios)
IF (ALLOCATED(da_RankScore1st)) DEALLOCATE(da_RankScore1st,STAT=ios)
IF (ALLOCATED(da_DualSoln1st )) DEALLOCATE(da_DualSoln1st ,STAT=ios)
IF (ALLOCATED(da_RankScore   )) DEALLOCATE(da_RankScore   ,STAT=ios)
IF (ALLOCATED(da_DualSoln    )) DEALLOCATE(da_DualSoln    ,STAT=ios)
RETURN
END SUBROUTINE htrsdpleast

!==============================================================================

SUBROUTINE htdpleast(i_NumObs, i_NumVars, d_Theta, i_NumPermut, i_RandNumSeed, &
                     i_NumToDrop, ia_PosToDrop, d_Toler, &
                     da_Data, &
                     d_To, d_PValue, &
                     da_Betas, da_RedBetas, &
                     da_Resids, da_RedResids, &
                     d_SumAbsValResRed, d_WtSumAbsDevsRedMod, &
                     i_Iter, i_ExitCode, iEr,ch_MyMsg,da_STV,l_SaveTest)
!___Imported Parameters and Variables:

USE blcmnmod, ONLY: d_ONE, d_ZERO, i_NOT_OK, i_OK
        ! ^-- Some global Blossom variables and parameters.
USE jonsmodule, ONLY: i_CCode, i_REGSRMOD_ER005, ch_EM00033, ch_REGRSMOD_EM020
        ! ^-- Message handling, etc.
!___Imported Procedures:
USE binomod, ONLY: random_binomial
        ! ^-- random_binomial: random binomial deviate
USE jonsmodule, ONLY: errhand
       
USE mt19937, ONLY: init_genrand
        ! ^-- Mersenne Twister Pseudo Random Number Generator
IMPLICIT NONE
!___Dummy Arguments:
INTEGER,          INTENT(IN)    :: i_NumObs
INTEGER,          INTENT(IN)    :: i_NumVars
REAL (KIND(0D0)), INTENT(IN)    :: d_Theta
INTEGER,          INTENT(IN)    :: i_NumPermut
INTEGER,          INTENT(IN)    :: i_RandNumSeed
INTEGER,          INTENT(IN)    :: i_NumToDrop
INTEGER,          INTENT(IN)    :: ia_PosToDrop(:)
REAL (KIND(0D0)), INTENT(IN)    :: d_Toler
REAL (KIND(0D0)), INTENT(INOUT) :: da_Data(:,:)
REAL (KIND(0D0)), INTENT(OUT)   :: d_To
REAL (KIND(0D0)), INTENT(OUT)   :: d_PValue
REAL (KIND(0D0)), INTENT(OUT)   :: da_Betas(:)
REAL (KIND(0D0)), INTENT(OUT)   :: da_RedBetas(:)
REAL (KIND(0D0)), INTENT(OUT)   :: da_Resids(:)
REAL (KIND(0D0)), INTENT(OUT)   :: da_RedResids(:)
REAL (KIND(0D0)), INTENT(OUT)   :: d_SumAbsValResRed
REAL (KIND(0D0)), INTENT(OUT)   :: d_WtSumAbsDevsRedMod
INTEGER,          INTENT(OUT)   :: i_Iter
INTEGER,          INTENT(OUT)   :: i_ExitCode
INTEGER,          INTENT(OUT)   :: iEr
INTEGER, PARAMETER :: i_LEN_MY_EMSG = 254 ! needs to be this big for output of var list in dostatus.
CHARACTER (LEN=i_LEN_MY_EMSG), INTENT(OUT) :: ch_MyMsg
REAL (KIND(0D0)), INTENT(OUT)  :: da_STV(:) ! save test values
LOGICAL,          INTENT(IN)     :: l_SaveTest
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
!     SUBROUTINE HTDPLEAST
! DESCRIPTION:
!     Perform hypothesis test of lad or quantile regression with the
!     double permutation option (no rank scores).  Return results for
!     reduced model and test.
!
! HYP / DP ..............................................(HYP_DOUBLE_PERM)
!    Condition: 1. First Command was LAD / QUANT=t
!               2. HYP command with reduced model
!               3. HYP / DP (Double Permutation)
!               4. No Rankscore
!
!       Action: LAD / QUANT - >
!               -> L1 -> T(obs), Resids -> | Little MEDQ Center | ->
!                                          |  permut(Resids)    |
!                                             \ (2...NPerms) /
!                                              \    loop    /
!                                               <--  L1  <--
!
!       Output:  Betas,
!                T(n)
!                p-Value
!
! LANGUAGE:
!     Fortran 90/95
! AUTHOR:
!     Jon D. Richards, Operations Research Analyst    jon_richards@usgs.gov
!     Fort Collins Science Center                  http://www.fort.usgs.gov
!     U.S. Geological Survey  -  Biological Resources Division
! MODIFICATION HISTORY:
!   CODE HISTORY:
!     CREATED - JDR Jan 2004
!     Mod - Added option to have SAVETEST of regression test statistics for
!           original and permuted regressions to a file - JDR  24 Mar 2005
!     Mod - Add option to use Mersenne Twister Pseudo Random Number Generator
!           Initialize both PRNGs at once. - JDR Apr 2005
!   COMMENT HISTORY:
!     CREATED - JDR Jan 2004!
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
!___Local Variables:
REAL (KIND(0D0)), ALLOCATABLE :: da_DataSave(:,:)
REAL (KIND(0D0)), ALLOCATABLE :: da_DataRed(:,:)
REAL (KIND(0D0)), ALLOCATABLE :: da_DepVarVals(:)
REAL (KIND(0D0)), ALLOCATABLE :: da_DepVarValsRed(:)
REAL (KIND(0D0)), ALLOCATABLE :: da_RedResids1st(:)
REAL (KIND(0D0)), ALLOCATABLE :: da_RedBetasLocal(:)
REAL (KIND(0D0)), ALLOCATABLE :: da_BetasLocal(:)
REAL (KIND(0D0)) :: d_QatTStar, d_SumAbsValRes, d_SumAbsValResRed1st, &
      d_TauStar, d_TempTheta, d_Tstar, d_WtSumAbsDevsFulMod, &
      d_WtSumAbsDevsFulMod1st, d_WtSumAbsDevsRedMod1st

INTEGER, ALLOCATABLE :: ia_Work(:)
INTEGER :: i_Temp, i, ii, i_Cnt2Perm, i_JCount, ios, i_Sz, j
INTEGER :: i_NumVarsP1, i_NumVarsP2, i_NumObsP1, i_NumObsP2, i_NumVRed, &
      i_NumVRedP1, i_NumVRedP2
LOGICAL :: l_First, l_HaveZero, l_TrueLad
!___Intrinsic Procedures:
INTRINSIC :: ABS, ALLOCATED, DBLE, HUGE, INT, RANDOM_SEED, REAL
!___Executable Statements:
!d CALL MyDebug(">> into HTDPLEAST")
                               ! debug!
iEr = i_OK ! no problem yet
i_Iter = 0
i_ExitCode = -HUGE(1) ! absurd value
d_To                 = -HUGE(d_ONE) ! absurd value
d_PValue             = -HUGE(d_ONE) ! absurd value
d_SumAbsValResRed    = -HUGE(d_ONE) ! absurd value
d_WtSumAbsDevsRedMod = -HUGE(d_ONE) ! absurd value
! Initialize default PRNG
CALL RANDOM_SEED(SIZE=i_Sz)
CALL RANDOM_SEED(PUT=(/(i_RandNumSeed,i=1,i_Sz)/))
! Initialize Mersenne Twister PRNG
CALL init_genrand(i_RandNumSeed)
IF (d_Theta < d_ZERO) THEN
   l_TrueLad = .TRUE.
   d_TempTheta = -d_Theta
ELSE
   d_TempTheta = d_Theta
   l_TrueLad = .FALSE.
END IF
i_NumVarsP1   = i_NumVars + 1
i_NumVarsP2   = i_NumVars + 2
i_NumObsP1    = i_NumObs  + 1
i_NumObsP2    = i_NumObs  + 2
i_NumVRed     = i_NumVars - i_NumToDrop
i_NumVRedP1   = i_NumVRed + 1
i_NumVRedP2   = i_NumVRed + 2



ALLOCATE(da_DataSave(1:i_NumObsP2,1:i_NumVarsP2), &
          da_DataRed(1:i_NumObsP2,1:i_NumVRedP2), & ! reduced data array
             ia_Work(1:i_NumObs),  &
       da_BetasLocal(1:i_NumVars), &
       da_DepVarVals(1:i_NumObs),  &
    da_DepVarValsRed(1:i_NumObs),  & ! reduced dep var vals
     da_RedResids1st(1:i_NumObs),  & ! copy of red Mod resids
    da_RedBetasLocal(1:i_NumVRed), & ! betas of red mod 1st time
                     STAT=ios)
IF (ios /= i_OK) THEN ! Memory allocation error: da_DataRed,da_DepVarValsRed,da_RedResids1st in LEAST
   CALL errhand(ch_MyMsg,i_Code=ios, &
                ch_Msg=ch_EM00033, &
                ch_S1="dynamic arrays", &
                ch_S2="htdpleast")
   iEr = i_NOT_OK
   GOTO 80000
END IF
da_DataRed = d_ZERO
da_DepVarValsRed = d_ZERO
da_RedResids1st = d_ZERO
da_RedBetasLocal = d_ZERO
da_DataSave(1:i_NumObs,1:i_NumVarsP1) = da_Data(1:i_NumObs,1:i_NumVarsP1)
! ............................................................................
i_JCount = 1
l_HaveZero = .FALSE.
DO j=1,i_NumVarsP1
   IF (j == i_NumVarsP1) THEN
      da_DataRed(1:i_NumObs,i_JCount) = da_Data(1:i_NumObs,j) ! this is dependent var
   ELSE
      IF (ia_PosToDrop(j) == 0) THEN
         da_DataRed(1:i_NumObs,i_JCount) = da_Data(1:i_NumObs,j) ! indep var values
         i_JCount = i_JCount + 1
      END IF
   END IF
   call rchkusr()
END DO


d_WtSumAbsDevsRedMod1st = d_ZERO
d_WtSumAbsDevsFulMod1st = d_ZERO
d_SumAbsValResRed1st    = d_ZERO
da_DepVarVals(1:i_NumObs) = da_Data(1:i_NumObs,i_NumVarsP1)
da_DepVarValsRed(1:i_NumObs) = da_DataRed(1:i_NumObs,i_NumVRedP1)
!--------------------- Begin calls to L1 routine --------------------- !
CALL L1(i_NumObs, i_NumVars, i_NumObsP2, i_NumVarsP2,    da_Data,         &
      da_DepVarVals,    d_Toler, da_BetasLocal, da_Resids,       ia_Work, &
      d_TempTheta, l_TrueLad)
da_Betas = da_BetasLocal
CALL L1(i_NumObs, i_NumVRed, i_NumObsP2, i_NumVRedP2,    da_DataRed,      &
      da_DepVarValsRed, d_Toler, da_RedBetas,   da_RedResids1st, ia_Work, &
      d_TempTheta, l_TrueLad)
i_ExitCode = INT(da_DataRed(i_NumObsP2,i_NumVRedP1))
i_Iter     = INT(da_DataRed(i_NumObsP2,i_NumVRedP2))
!----------------------- End calls to L1 routine --------------------- !
d_SumAbsValRes = da_Data(i_NumObsP1,i_NumVarsP1)
d_SumAbsValResRed1st = da_DataRed(i_NumObsP1,i_NumVRedP1)
!----July 13 1998 new code to properly accumulate Weighted sums of the
! absolute deviations for *QUANTILE Regressions*==>(Quant is positive)
IF (.NOT.l_TrueLad) THEN ! Quantile Regression:
   d_WtSumAbsDevsRedMod1st = d_ZERO
   DO i=1,i_NumObs
      IF (da_RedResids1st(i) < d_ZERO) THEN ! if resid. is negative
         d_WtSumAbsDevsRedMod1st = d_WtSumAbsDevsRedMod1st + da_RedResids1st(i)*(d_TempTheta-d_ONE)
      ELSE                      ! if resid. is positive
         d_WtSumAbsDevsRedMod1st = d_WtSumAbsDevsRedMod1st + da_RedResids1st(i)*d_TempTheta
      END IF
   END DO
   d_WtSumAbsDevsFulMod1st = d_ZERO
   DO i=1,i_NumObs   ! Full Model, Quantile Regression
      IF (da_Resids(i) < d_ZERO) THEN    ! if resid. is negative
         d_WtSumAbsDevsFulMod1st = d_WtSumAbsDevsFulMod1st + da_Resids(i)*(d_TempTheta-d_ONE)
      ELSE                      ! if resid. is positive !
         d_WtSumAbsDevsFulMod1st = d_WtSumAbsDevsFulMod1st + da_Resids(i)*d_TempTheta
      END IF
      call rchkusr()
   END DO
END IF ! (l_DoQuantRegression)
!---- end July 13 1998 code segment -------------------------------- !
IF (l_TrueLad) THEN ! Quantile Regression
   IF (d_SumAbsValRes <= d_SMALL_VALUE) THEN ! NO RESIDUAL ERRORS d_SumAbsValRes found in htdpleast
      l_HaveZero = .TRUE.    ! Don't divide by Zero... a
      d_Tstar = d_LARGE_VALUE
   ELSE       ! TestStat!
      d_Tstar = (d_SumAbsValResRed1st-d_SumAbsValRes) / d_SumAbsValRes
   END IF
ELSE ! not a true LAD, a quantile regression
   IF (d_WtSumAbsDevsFulMod1st <= d_SMALL_VALUE) THEN
      l_HaveZero = .TRUE.    ! Don't divide by Zero... a
      d_Tstar = d_LARGE_VALUE
   ELSE
      d_Tstar = (d_WtSumAbsDevsRedMod1st-d_WtSumAbsDevsFulMod1st) / d_WtSumAbsDevsFulMod1st
   END IF
END IF

IF (l_HaveZero) THEN
   ! send message back to caller that there are no residual errors in model,
   ! no need to test

!  _______________________________________
! |                                       |
! | Perfect fit, zero residuals in model. |
! | We can't test this any further.       |
! |_______________________________________|
!
   CALL errhand(ch_MyMsg,ch_Msg=ch_REGRSMOD_EM020)
   iEr = i_NOT_OK
   d_PValue = d_ONE
   d_To = d_ZERO
   GOTO 80000
END IF
! ............................................................................
d_To = d_Tstar  ! WE REPORT THIS BACK TO CALLER
l_First = .TRUE. ! to initialize random_binomial function
i_Cnt2Perm = 1
IF (l_SAVETEST) THEN ! We want to save original and all resampled test statistics
   da_STV(1) = d_To
END IF
  ! vvvvvvvvvv   B E G I N   P E R M U T A T I O N   L O O P   vvvvvvvvvv
DO i=2,i_NumPermut
   da_Data(1:i_NumObs,1:i_NumVarsP1) = da_DataSave(1:i_NumObs,1:i_NumVarsP1) ! data is destroyed by L1 replace from saved data
   i_Temp = random_binomial(i_NumObs, REAL(ABS(d_TempTheta)), l_First)
   l_First = .FALSE.
   d_TauStar = DBLE(i_Temp)/DBLE(i_NumObs)
   d_QatTStar = littlemedq1(i_NumObs, da_RedResids1st, d_TauStar) ! random binomial variate centered on taustar
   da_RedResids(1:i_NumObs) = da_RedResids1st(1:i_NumObs) - d_QatTStar ! center the residuals
   CALL permut(da_RedResids, i_NumObs)
   da_Data(1:i_NumObs,i_NumVarsP1) = da_RedResids(1:i_NumObs)
   ! ............................................................................
   l_HaveZero = .FALSE. ! initial non-0 sums of abs vals of resids
   ! Fill an da_Data data matrix for reduced model. Also, remember that
   ! the last column in da_Data matrix is dependent variable.
   ! If we have a reduced model, we will drop some variables.
   i_JCount = 1
   DO j=1,i_NumVarsP1
      IF (j == i_NumVarsP1) THEN
         da_DataRed(1:i_NumObs,i_JCount) = da_Data(1:i_NumObs,j) ! this is dependent var
      ELSE
         IF (ia_PosToDrop(j) == 0) THEN
            da_DataRed(1:i_NumObs,i_JCount) = da_Data(1:i_NumObs,j) ! indep var values
            i_JCount = i_JCount + 1
         END IF
      END IF
      call rchkusr()
   END DO
   d_WtSumAbsDevsRedMod = d_ZERO
   d_WtSumAbsDevsFulMod = d_ZERO
   d_SumAbsValResRed    = d_ZERO
   da_DepVarVals(1:i_NumObs) = da_Data(1:i_NumObs,i_NumVarsP1)   ! keep copies of these
   da_DepVarValsRed(1:i_NumObs) = da_DataRed(1:i_NumObs,i_NumVRedP1)
   !--------------------- Begin calls to L1 routine --------------------- !
   CALL L1(i_NumObs, i_NumVars, i_NumObsP2, i_NumVarsP2,    da_Data,         &
         da_DepVarVals,    d_Toler, da_BetasLocal,    da_Resids,    ia_Work, &
         d_TempTheta, l_TrueLad)
   CALL L1(i_NumObs, i_NumVRed, i_NumObsP2, i_NumVRedP2,    da_DataRed,      &
         da_DepVarValsRed, d_Toler, da_RedBetasLocal, da_RedResids, ia_Work, &
         d_TempTheta, l_TrueLad)
   !----------------------- End calls to L1 routine --------------------- !
   ! sum of abs value of residuals, from subroutine L1
   d_SumAbsValRes = da_Data(i_NumObsP1,i_NumVarsP1)         ! for full model
   d_SumAbsValResRed = da_DataRed(i_NumObsP1,i_NumVRedP1) ! same for red mod
   !----July 13 1998 new code to properly accumulate Weighted sums of the
   ! absolute deviations for *QUANTILE Regressions*==>(Quant is positive)
   IF (.NOT.l_TrueLad) THEN  ! Quantile regression:
      d_WtSumAbsDevsRedMod = d_ZERO
      DO ii=1,i_NumObs
         IF (da_RedResids(ii) < d_ZERO) THEN ! if resid. is negative
            d_WtSumAbsDevsRedMod = d_WtSumAbsDevsRedMod + da_RedResids(ii)*(d_TempTheta-d_ONE)
         ELSE                      ! if resid. is positive
            d_WtSumAbsDevsRedMod = d_WtSumAbsDevsRedMod + da_RedResids(ii)*d_TempTheta
         END IF
      END DO
      ! Full Model, Quantile Regression
      d_WtSumAbsDevsFulMod = d_ZERO
      DO ii=1,i_NumObs
         IF (da_Resids(ii) < d_ZERO) THEN    ! if resid. is negative
            d_WtSumAbsDevsFulMod = d_WtSumAbsDevsFulMod + da_Resids(ii)*(d_TempTheta-d_ONE)
         ELSE                      ! if resid. is positive !
            d_WtSumAbsDevsFulMod = d_WtSumAbsDevsFulMod + da_Resids(ii)*d_TempTheta
         END IF
         call rchkusr()
      END DO
   END IF
   !---- end July 13 1998 code segment -------------------------------- !
   ! if d_SumAbsValRes = 0 then we don't want to divide by it.  In fact if the
   ! sum of the absolute values of the residuals is zero, we have a
   ! perfect fit and the P-value is 1.0; we don't need to go through the
   ! contortions of the permutations.  We can just put out our P-value
   ! and put out "Undefined" as value of the test statistic d_Tstar
   ! (aka Tee-zero).
   IF (l_TrueLad) THEN ! true lad regression:
      IF (d_SumAbsValRes <= d_SMALL_VALUE) THEN
         l_HaveZero = .TRUE.    ! Don't divide by Zero... a
         d_Tstar = d_LARGE_VALUE
      ELSE       ! TestStat!
         d_Tstar = (d_SumAbsValResRed-d_SumAbsValRes) / d_SumAbsValRes
      END IF
   ELSE ! not a true LAD, a quantile regression
      IF (d_WtSumAbsDevsFulMod <= d_SMALL_VALUE) THEN
         l_HaveZero = .TRUE.    ! Don't divide by Zero... a
         d_Tstar = d_LARGE_VALUE
      ELSE
         d_Tstar = (d_WtSumAbsDevsRedMod-d_WtSumAbsDevsFulMod) / d_WtSumAbsDevsFulMod
      END IF
   END IF
   IF (l_SAVETEST) THEN  ! Save value of test stat for this permuted sample
      da_STV(i) = d_Tstar
   END IF
   ! ............................................................................
   IF (d_Tstar >= d_To) THEN
      i_Cnt2Perm = i_Cnt2Perm + 1  ! THIS IS WHAT COUNTS...
   END IF
   call rchkusr()
END DO

! /////////////////////////////////////////////////////////////////////////////////////////////
!
! Check to see that we have access to output file
!
! /////////////////////////////////////////////////////////////////////////////////////////////



  ! ^^^^^^^^^^^^^^   E N D   P E R M U T A T I O N   L O O P   ^^^^^^^^^^
d_PValue = DBLE(i_Cnt2Perm)/DBLE(i_NumPermut)
d_SumAbsValResRed = d_SumAbsValResRed1st
d_WtSumAbsDevsRedMod = d_WtSumAbsDevsRedMod1st
80000 CONTINUE

IF (ALLOCATED(da_RedBetasLocal)) DEALLOCATE(da_RedBetasLocal,STAT=ios)
IF (ALLOCATED(da_RedResids1st )) DEALLOCATE(da_RedResids1st ,STAT=ios)
IF (ALLOCATED(da_DepVarValsRed)) DEALLOCATE(da_DepVarValsRed,STAT=ios)
IF (ALLOCATED(da_DepVarVals   )) DEALLOCATE(da_DepVarVals   ,STAT=ios)
IF (ALLOCATED(da_BetasLocal   )) DEALLOCATE(da_BetasLocal,   STAT=ios)
IF (ALLOCATED(ia_Work         )) DEALLOCATE(ia_Work         ,STAT=ios)
IF (ALLOCATED(da_DataRed      )) DEALLOCATE(da_DataRed      ,STAT=ios)
IF (ALLOCATED(da_DataSave     )) DEALLOCATE(da_DataSave     ,STAT=ios)
RETURN
END SUBROUTINE htdpleast

!==============================================================================

SUBROUTINE htqm1dpleast(i_NumObs, i_NumVars, d_Theta, i_NumPermut, i_RandNumSeed, &
                        i_NumToDrop, ia_PosToDrop, d_Toler, &
                        da_Data, &
                        d_To, d_PValue, &
                        da_Betas, da_RedBetas, &
                        da_Resids, da_RedResids, &
                        d_SumAbsValResRed, d_WtSumAbsDevsRedMod, &
                        i_Iter, i_ExitCode, iEr,ch_MyMsg,da_STV,l_SaveTest)
!___Imported Parameters and Variables:

USE blcmnmod, ONLY: d_ONE, d_ZERO, i_NOT_OK, i_OK
        ! ^-- Some global Blossom variables and parameters.
USE jonsmodule, ONLY: i_CCode, i_REGSRMOD_ER005, ch_EM00033, ch_REGRSMOD_EM020
        ! ^-- Message handling, etc.
!___Imported Procedures:
USE binomod, ONLY: random_binomial
        ! ^-- random_binomial: random binomial deviate
USE jonsmodule, ONLY: errhand
       
USE mt19937, ONLY: init_genrand
        ! ^-- Mersenne Twister Pseudo Random Number Generator
IMPLICIT NONE
!___Dummy Arguments:
INTEGER,          INTENT(IN)    :: i_NumObs
INTEGER,          INTENT(IN)    :: i_NumVars
REAL (KIND(0D0)), INTENT(IN)    :: d_Theta
INTEGER,          INTENT(IN)    :: i_NumPermut
INTEGER,          INTENT(IN)    :: i_RandNumSeed
INTEGER,          INTENT(IN)    :: i_NumToDrop
INTEGER,          INTENT(IN)    :: ia_PosToDrop(:)
REAL (KIND(0D0)), INTENT(IN)    :: d_Toler
REAL (KIND(0D0)), INTENT(INOUT) :: da_Data(:,:)
REAL (KIND(0D0)), INTENT(OUT)   :: d_To
REAL (KIND(0D0)), INTENT(OUT)   :: d_PValue
REAL (KIND(0D0)), INTENT(OUT)   :: da_Betas(:)
REAL (KIND(0D0)), INTENT(OUT)   :: da_RedBetas(:)
REAL (KIND(0D0)), INTENT(OUT)   :: da_Resids(:)
REAL (KIND(0D0)), INTENT(OUT)   :: da_RedResids(:)
REAL (KIND(0D0)), INTENT(OUT)   :: d_SumAbsValResRed
REAL (KIND(0D0)), INTENT(OUT)   :: d_WtSumAbsDevsRedMod
INTEGER,          INTENT(OUT)   :: i_Iter
INTEGER,          INTENT(OUT)   :: i_ExitCode
INTEGER,          INTENT(OUT)   :: iEr
INTEGER, PARAMETER :: i_LEN_MY_EMSG = 254 ! needs to be this big for output of var list in dostatus.
CHARACTER (LEN=i_LEN_MY_EMSG), INTENT(OUT) :: ch_MyMsg
REAL (KIND(0D0)), INTENT(OUT) :: da_STV(:) ! save test values
LOGICAL,          INTENT(IN)     :: l_SaveTest
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
!     SUBROUTINE HTQM1DPLEAST
! DESCRIPTION:
!     Perform hypothesis test of lad or quantile regression where the number
!     of parameters in the reduced model is greater than 1 and there is no
!     specification of rank score and there IS double permutation.
!     Drop appropriate number of observations with zero-valued residuals
!     for the permutation test, and report results of the reduced model and test.
!

! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
!___Local Parameters:
!___Local Variables:
REAL (KIND(0D0)), ALLOCATABLE :: da_DataQM1(:,:)
REAL (KIND(0D0)), ALLOCATABLE :: da_DataRed(:,:)
REAL (KIND(0D0)), ALLOCATABLE :: da_DataSave(:,:)
REAL (KIND(0D0)), ALLOCATABLE :: da_DepVarVals(:)
REAL (KIND(0D0)), ALLOCATABLE :: da_DepVarValsRed(:)
REAL (KIND(0D0)), ALLOCATABLE :: da_RedBetasLocal(:)
REAL (KIND(0D0)), ALLOCATABLE :: da_RedResidsQM1(:)
REAL (KIND(0D0)), ALLOCATABLE :: da_ResidsQM1(:)
REAL (KIND(0D0)), ALLOCATABLE :: da_RedResids1st(:)
REAL (KIND(0D0)), ALLOCATABLE :: da_ResRedDandP(:)

REAL (KIND(0D0)), ALLOCATABLE :: da_BetasLocal(:)
REAL (KIND(0D0)) :: d_SumAbsValRes, d_SumAbsValResRed1st, d_TempTheta, &
      d_To_LQ, d_WtSumAbsDevsFulMod, d_WtSumAbsDevsRedMod1st
REAL (KIND(0D0)) :: d_TauStar, d_QatTStar !xxx , d_Tstar
INTEGER, ALLOCATABLE :: ia_DropZero(:)
INTEGER, ALLOCATABLE :: ia_Work(:)
INTEGER :: i, i1, i_Cnt, i_Cnt2Perm, i_JCount, i_NumObsP1, i_NumObsP2, &
      i_NumObsQM1, i_NumObsQM1P1, i_NumObsQM1P2, i_NumVarsP1, i_NumVarsP2, &
      i_NumVRed, i_NumVRedP1, i_NumVRedP2, i_Qm1, ios, i_Sz, j
INTEGER :: i_Temp
!d INTEGER :: id
LOGICAL :: l_HaveZero, l_TrueLad, l_First

!___Intrinsic Procedures:
INTRINSIC :: ALLOCATED, DBLE, HUGE, INT, RANDOM_SEED

                                                    ! debug!
iEr = i_OK ! no problem yet
i_Iter = 0
i_ExitCode           = -HUGE(1)     ! absurd value
d_To                 = -HUGE(d_ONE) ! absurd value
d_PValue             = -HUGE(d_ONE) ! absurd value
d_SumAbsValResRed    = -HUGE(d_ONE) ! absurd value
d_WtSumAbsDevsRedMod = -HUGE(d_ONE) ! absurd value
! Initialize default PRNG
CALL RANDOM_SEED(SIZE=i_Sz)
CALL RANDOM_SEED(PUT=(/(i_RandNumSeed,i=1,i_Sz)/))
! Initialize Mersenne Twister PRNG
CALL init_genrand(i_RandNumSeed)
IF (d_Theta < d_ZERO) THEN
   l_TrueLad = .TRUE.
   d_TempTheta = -d_Theta
ELSE
   d_TempTheta = d_Theta
   l_TrueLad = .FALSE.
END IF
i_NumVarsP1   = i_NumVars + 1
i_NumVarsP2   = i_NumVars + 2
i_NumObsP1    = i_NumObs  + 1
i_NumObsP2    = i_NumObs  + 2
i_NumVRed     = i_NumVars - i_NumToDrop
i_NumVRedP1   = i_NumVRed + 1
i_NumVRedP2   = i_NumVRed + 2
i_Qm1         = i_NumVRed - 1
i_NumObsQM1   = i_NumObs  - i_Qm1
i_NumObsQM1P1 = i_NumObsQM1 + 1
i_NumObsQM1P2 = i_NumObsQM1 + 2
ALLOCATE(da_RedResids1st(1:i_NumObs),  &
        da_RedBetasLocal(1:i_NumVRed), &
           da_BetasLocal(1:i_NumVars), &
                 ia_Work(1:i_NumObs),  &
             da_DataSave(1:i_NumObsP2,1:i_NumVarsP2), &
           da_DepVarVals(1:i_NumObs),  &
        da_DepVarValsRed(1:i_NumObs),  &
              da_DataRed(1:i_NumObsP2,1:i_NumVRedP2), &
            STAT=ios)
IF (ios /= i_OK) THEN
   CALL errhand(ch_MyMsg,i_Code=ios, &
                ch_Msg=ch_EM00033, &
                ch_S1="dynamic arrays", &
                ch_S2="htqm1dpleast")
   iEr = i_NOT_OK
   GOTO 80000
END IF
da_DataSave(1:i_NumObs,1:i_NumVarsP1) = da_Data(1:i_NumObs,1:i_NumVarsP1)
l_HaveZero = .FALSE. ! initial non-0 sums of abs vals of resids
! Fill an da_Data data matrix for reduced model. Also, remember that
! the last column in da_Data matrix is dependent variable.
! If we have a reduced model, we will drop some variables.
i_JCount = 1
DO j=1,i_NumVarsP1
   IF (j == i_NumVarsP1) THEN
      da_DataRed(1:i_NumObs,i_JCount) = da_Data(1:i_NumObs,j) ! this is dependent var
   ELSE
      IF (ia_PosToDrop(j) == 0) THEN
         da_DataRed(1:i_NumObs,i_JCount) = da_Data(1:i_NumObs,j) ! indep var values
         i_JCount = i_JCount + 1
      END IF
   END IF
   call rchkusr()
END DO
d_SumAbsValResRed    = d_ZERO
da_DepVarVals(1:i_NumObs) = da_Data(1:i_NumObs,i_NumVarsP1) ! keep copies
da_DepVarValsRed(1:i_NumObs) = da_DataRed(1:i_NumObs,i_NumVRedP1)
!--------------------- Begin calls to L1 routine --------------------- !
CALL L1(i_NumObs, i_NumVars, i_NumObsP2, i_NumVarsP2, da_Data, &
      da_DepVarVals,    d_Toler, da_BetasLocal,    da_Resids,  &
      ia_Work, d_TempTheta, l_TrueLad)
da_Betas = da_BetasLocal
CALL L1(i_NumObs, i_NumVRed, i_NumObsP2, i_NumVRedP2, da_DataRed, &
      da_DepVarValsRed, d_Toler, da_RedBetasLocal, da_RedResids,  &
      ia_Work, d_TempTheta, l_TrueLad)
da_RedBetas(1:i_NumVRed) = da_RedBetasLocal(1:i_NumVRed)
!----------------------- End calls to L1 routine --------------------- !
i_ExitCode = INT(da_DataRed(i_NumObsP2,i_NumVRedP1))
i_Iter     = INT(da_DataRed(i_NumObsP2,i_NumVRedP2))
d_SumAbsValRes = da_Data(i_NumObsP1,i_NumVarsP1)       ! for full model
d_SumAbsValResRed = da_DataRed(i_NumObsP1,i_NumVRedP1) ! same for red mod
 ! accumulate Weighted sums of the absolute deviations for *QUANTILE
 ! Regressions*==>(Quant is positive)
IF (.NOT.l_TrueLad) THEN ! Quantile Regression:
   d_WtSumAbsDevsRedMod = d_ZERO
   DO i=1,i_NumObs
      IF (da_RedResids(i) < d_ZERO) THEN ! if resid. is negative
         d_WtSumAbsDevsRedMod = d_WtSumAbsDevsRedMod + da_RedResids(i)*(d_TempTheta-d_ONE)
      ELSE                      ! if resid. is positive
         d_WtSumAbsDevsRedMod = d_WtSumAbsDevsRedMod + da_RedResids(i)*d_TempTheta
      END IF
   END DO
   d_WtSumAbsDevsFulMod = d_ZERO ! Full Model, Quantile Regression
   DO i=1,i_NumObs
      IF (da_Resids(i) < d_ZERO) THEN ! if resid. is negative
         d_WtSumAbsDevsFulMod = d_WtSumAbsDevsFulMod + da_Resids(i)*(d_TempTheta-d_ONE)
      ELSE                      ! if resid. is positive !
         d_WtSumAbsDevsFulMod = d_WtSumAbsDevsFulMod + da_Resids(i)*d_TempTheta
      END IF
   END DO
END IF
d_WtSumAbsDevsRedMod1st = d_WtSumAbsDevsRedMod
! if d_SumAbsValRes = 0 then we don't want to divide by it.  In fact if tF1
! sum of the absolute values of the residuals is zero, we have a
! perfect fit and the P-value is 1.0; we don't need to go through the
! contortions of the permutations.  We can just put out our P-value
! and put out "Undefined" as value of the test statistic d_To_LQ
! (aka Tee-zero).
IF (l_TrueLad) THEN
   IF (d_SumAbsValRes <= d_SMALL_VALUE) THEN
      l_HaveZero = .TRUE.  ! Don't divide by Zero... a
   ELSE       ! TestStat:
      d_To_LQ = (d_SumAbsValResRed-d_SumAbsValRes) / d_SumAbsValRes
   END IF
ELSE ! not a true LAD, a quantile regression
   IF (d_WtSumAbsDevsFulMod <= d_SMALL_VALUE) THEN
      l_HaveZero = .TRUE.  ! Don't divide by Zero... a
   ELSE       ! TestStat:
      d_To_LQ = (d_WtSumAbsDevsRedMod-d_WtSumAbsDevsFulMod) / d_WtSumAbsDevsFulMod
   END IF
END IF
IF (l_HaveZero) THEN
   d_PValue = d_ONE
   d_SumAbsValResRed = d_ZERO
   d_WtSumAbsDevsRedMod = d_ZERO
!  _______________________________________
! |                                       |
! | Perfect fit, zero residuals in model. |
! | We can't test this any further.       |
! |_______________________________________|
!
   CALL errhand(ch_MyMsg,ch_Msg=ch_REGRSMOD_EM020)
   iEr = i_NOT_OK
   GOTO 80000 !...  RETURN TO CALLING PROCEDURE, no need to test.
END IF
d_SumAbsValResRed1st = d_SumAbsValResRed
!---------------------------------------------------------------------
da_RedResids1st(1:i_NumObs) = da_RedResids(1:i_NumObs)
da_Data(1:i_NumObs,1:i_NumVarsP1) = da_DataSave(1:i_NumObs,1:i_NumVarsP1)
d_To = d_To_LQ
IF (ALLOCATED(da_DataRed      )) DEALLOCATE(da_DataRed      ,STAT=ios)
IF (ALLOCATED(da_DepVarValsRed)) DEALLOCATE(da_DepVarValsRed,STAT=ios)
IF (ALLOCATED(da_DepVarVals   )) DEALLOCATE(da_DepVarVals   ,STAT=ios)
IF (ALLOCATED(ia_Work         )) DEALLOCATE(ia_Work         ,STAT=ios)
ALLOCATE(da_DataQM1(1:i_NumObsQM1P2,1:i_NumVarsP2), &
         da_DataRed(1:i_NumObsQM1P2,1:i_NumVRedP2), &
       da_ResidsQM1(1:i_NumObsQM1), &
      da_DepVarVals(1:i_NumObsQM1), &
    da_RedResidsQM1(1:i_NumObsQM1), &
   da_DepVarValsRed(1:i_NumObsQM1), &
     da_ResRedDandP(1:i_NumObsQM1), &
        ia_DropZero(1:i_NumObs),    &
            ia_Work(1:i_NumObs),    &
               STAT=ios)
IF (ios /= i_OK) THEN
   CALL errhand(ch_MyMsg,i_Code=ios,            &
                ch_Msg=ch_EM00033,        &
                 ch_S1="dynamic arrays",  &
                 ch_S2="htqm1least")
   iEr = i_NOT_OK
   GOTO 80000
END IF
i_Cnt = 0
j = 1
DO i=1,i_NumObs
   IF (i_Cnt < i_Qm1  .AND.  da_RedResids1st(i) == d_ZERO) THEN
      ia_DropZero(i) = 1
      i_Cnt = i_Cnt + 1
   ELSE
      da_ResRedDandP(j) = da_RedResids1st(i)
      j = j + 1
      ia_DropZero(i) = 0
   END IF
END DO

! Now we need to fill the new data matrix with all but the DROPped
! observations.  We need all the matrices allocated to accomodate
! this new size.  Each time through the permutation loop, we need to
! pemute the full Reduced Residual vector and place it in the data
! array, dropping whichever observations (rows) that our dropzero
! values align with. Each permutation will result in some different
! row(s) being dropped.
i_Cnt2Perm = 1
CALL ipermut(ia_DropZero, i_NumObs) ! Permute the drop zero array
CALL permut(da_ResRedDandP, i_NumObsQM1) ! Permute the da_ResRedDandP array

i_Cnt = 0
DO i=1,i_NumObs ! Fill the data matrix da_DataQM1
   IF (ia_DropZero(i) == 0) THEN
      i_Cnt = i_Cnt + 1
      da_DataQM1(i_Cnt,1:i_NumVars) = da_Data(i,1:i_NumVars)
      da_DataQM1(i_Cnt,i_NumVarsP1) = da_ResRedDandP(i_Cnt)
   END IF
END DO

l_First = .TRUE. ! to initialize random_binomial function
IF (l_SAVETEST) THEN ! We want to save original and all resampled test statistics
   da_STV(1) = d_To
END IF
  ! vvvvvvvvvv   B E G I N   P E R M U T A T I O N   L O O P   vvvvvvvvvv
DO i=2,i_NumPermut

   !
   i_Temp = random_binomial(i_NumObsQM1, REAL(ABS(d_TempTheta)), l_First) ! a random binomial variate
   l_First = .FALSE. ! Now that we've been to random_binomial, it is no longer the first time there.
   d_TauStar = DBLE(i_Temp)/DBLE(i_NumObsQM1)
   d_QatTStar = littlemedq1(i_NumObsQM1, da_ResRedDandP, d_TauStar) ! random binomial variate centered on taustar
   !

   da_RedResidsQM1(1:i_NumObsQM1) = da_ResRedDandP(1:i_NumObsQM1) - d_QatTStar ! center the residuals

   CALL permut(da_RedResidsQM1, i_NumObsQM1)

   da_DataQM1(1:i_NumObsQM1,i_NumVarsP1) = da_RedResidsQM1(1:i_NumObsQM1) ! use these resids as dep. var. here
   l_HaveZero = .FALSE. ! initial non-0 sums of abs vals of resids
   ! Fill an da_Data data matrix for reduced model. Also, remember that
   ! the last column in da_Data matrix is dependent variable.
   ! If we have a reduced model, we will drop some variables.
   i_JCount = 1
   DO j=1,i_NumVarsP1
      IF (j == i_NumVarsP1) THEN
         da_DataRed(1:i_NumObsQM1,i_JCount) = da_DataQM1(1:i_NumObsQM1,j) ! this is dependent var
      ELSE
         IF (ia_PosToDrop(j) == 0) THEN
            da_DataRed(1:i_NumObsQM1,i_JCount) = da_DataQM1(1:i_NumObsQM1,j) ! indep var values
            i_JCount = i_JCount + 1
         END IF
      END IF
      call rchkusr()
   END DO
   d_WtSumAbsDevsRedMod = d_ZERO
   d_WtSumAbsDevsFulMod = d_ZERO
   d_SumAbsValResRed    = d_ZERO
   da_DepVarVals(1:i_NumObsQM1) = da_DataQM1(1:i_NumObsQM1,i_NumVarsP1)   ! keep copies
   da_DepVarValsRed(1:i_NumObsQM1) = da_DataRed(1:i_NumObsQM1,i_NumVRedP1)
   !--------------------- Begin calls to L1 routine --------------------- !
   CALL L1(i_NumObsQM1, i_NumVars, i_NumObsQM1P2, i_NumVarsP2, da_DataQM1, &
         da_DepVarVals,    d_Toler, da_BetasLocal,    da_ResidsQM1,        &
         ia_Work, d_TempTheta, l_TrueLad)
   CALL L1(i_NumObsQM1, i_NumVRed, i_NumObsQM1P2, i_NumVRedP2, da_DataRed, &
         da_DepVarValsRed, d_Toler, da_RedBetasLocal, da_RedResidsQM1,     &
         ia_Work, d_TempTheta, l_TrueLad)
   !----------------------- End calls to L1 routine --------------------- !
   ! sum of abs value of residuals, from subroutine L1
   d_SumAbsValRes =    da_DataQM1(i_NumObsQM1P1,i_NumVarsP1)         ! for full model
   d_SumAbsValResRed = da_DataRed(i_NumObsQM1P1,i_NumVRedP1) ! same for red mod
   ! accumulate Weighted sums of the absolute deviations for
   !   *QUANTILE Regressions* ==>(Quant is positive)
   IF (.NOT.l_TrueLad) THEN ! Quantile Regression:
      d_WtSumAbsDevsRedMod1st = d_ZERO
      DO i1=1,i_NumObsQM1
         IF (da_RedResidsQM1(i1) < d_ZERO) THEN ! if resid. is negative
            d_WtSumAbsDevsRedMod1st = d_WtSumAbsDevsRedMod1st + da_RedResidsQM1(i1)*(d_TempTheta-d_ONE)
         ELSE                      ! if resid. is positive
            d_WtSumAbsDevsRedMod1st = d_WtSumAbsDevsRedMod1st + da_RedResidsQM1(i1)*d_TempTheta
         END IF
         call rchkusr()
      END DO
      d_WtSumAbsDevsFulMod = d_ZERO ! Full Model, Quantile Regression
      DO i1=1,i_NumObsQM1
         IF (da_ResidsQM1(i1) < d_ZERO) THEN    ! if resid. is negative
            d_WtSumAbsDevsFulMod = d_WtSumAbsDevsFulMod + da_ResidsQM1(i1)*(d_TempTheta-d_ONE)
         ELSE                      ! if resid. is positive !
            d_WtSumAbsDevsFulMod = d_WtSumAbsDevsFulMod + da_ResidsQM1(i1)*d_TempTheta
         END IF
         call rchkusr()
      END DO
   END IF ! (l_DoQuantRegression)
   ! if d_SumAbsValRes = 0 then we don't want to divide by it.  In fact if the
   ! sum of the absolute values of the residuals is zero, we have a
   ! perfect fit and the P-value is 1.0; we don't need to go through the
   ! contortions of the permutations.  We can just put out our P-value
   ! and put out "Undefined" as value of the test statistic d_Tstar
   ! (aka Tee-zero).
   IF (l_TrueLad) THEN
      IF (d_SumAbsValRes <= d_SMALL_VALUE) THEN ! Don't divide by Zero...
         d_To_LQ = d_LARGE_VALUE ! make it big enough to increment i_Cnt2Perm
      ELSE       ! TestStat:
         d_To_LQ = (d_SumAbsValResRed-d_SumAbsValRes) / d_SumAbsValRes
      END IF
   ELSE ! not a true LAD, a quantile regression
      IF (d_WtSumAbsDevsFulMod <= d_SMALL_VALUE) THEN ! Don't divide by Zero...
         d_To_LQ = d_LARGE_VALUE ! make it big enough to increment i_Cnt2Perm
      ELSE       ! TestStat:
         d_To_LQ = (d_WtSumAbsDevsRedMod1st-d_WtSumAbsDevsFulMod) / d_WtSumAbsDevsFulMod
      END IF
   END IF
   IF (l_SAVETEST) THEN  ! Save value of test stat for this permuted sample
      da_STV(i) = d_To_LQ
   END IF
   IF (d_To_LQ >= d_To) THEN
      i_Cnt2Perm = i_Cnt2Perm + 1
   END IF
   CALL ipermut(ia_DropZero, i_NumObs) ! Permute the drop zero array
   CALL permut(da_ResRedDandP, i_NumObsQM1) ! Permute the ResRedDandP array
   !

   !
   i_Cnt = 0
   DO i1=1,i_NumObs           ! Fill the data matrix da_DataQM1
      IF (ia_DropZero(i1) == 0) THEN
         i_Cnt = i_Cnt + 1
         da_DataQM1(i_Cnt,1:i_NumVars) = da_DataSave(i1,1:i_NumVars)
         da_DataQM1(i_Cnt,i_NumVarsP1) = da_ResRedDandP(i_Cnt)
      END IF
      call rchkusr()
   END DO
END DO ! end of permutation loop

  ! ^^^^^^^^^^^^^^   E N D   P E R M U T A T I O N   L O O P   ^^^^^^^^^^
! Things to report:
d_PValue = DBLE(i_Cnt2Perm)/DBLE(i_NumPermut)
d_SumAbsValResRed = d_SumAbsValResRed1st
d_WtSumAbsDevsRedMod = d_WtSumAbsDevsRedMod1st
80000 CONTINUE
! deallocate array space:
IF (ALLOCATED(ia_Work         )) DEALLOCATE(ia_Work,         STAT=ios)
IF (ALLOCATED(ia_DropZero     )) DEALLOCATE(ia_DropZero,     STAT=ios)
IF (ALLOCATED(da_ResRedDandP  )) DEALLOCATE(da_ResRedDandP,  STAT=ios)
IF (ALLOCATED(da_DepVarValsRed)) DEALLOCATE(da_DepVarValsRed,STAT=ios)
IF (ALLOCATED(da_RedResidsQM1 )) DEALLOCATE(da_RedResidsQM1, STAT=ios)
IF (ALLOCATED(da_DepVarVals   )) DEALLOCATE(da_DepVarVals,   STAT=ios)
IF (ALLOCATED(da_ResidsQM1    )) DEALLOCATE(da_ResidsQM1,    STAT=ios)
IF (ALLOCATED(da_DataRed      )) DEALLOCATE(da_DataRed,      STAT=ios)
IF (ALLOCATED(da_DataQM1      )) DEALLOCATE(da_DataQM1,      STAT=ios)
IF (ALLOCATED(da_BetasLocal   )) DEALLOCATE(da_BetasLocal,   STAT=ios)
IF (ALLOCATED(da_RedBetasLocal)) DEALLOCATE(da_RedBetasLocal,STAT=ios)
IF (ALLOCATED(da_RedResids1st )) DEALLOCATE(da_RedResids1st, STAT=ios)
!d CALL MyDebug("<< out of HTQM1DPLEAST")                              ! debug!
RETURN
END SUBROUTINE htqm1dpleast

!==============================================================================

SUBROUTINE ipermut(X, N)
!___Imported Parameters and Variables:
        ! ^-- Some global Blossom variables and parameters.
!___Imported Procedures:
USE mt19937, ONLY: genrand_real3
        ! ^-- Mersenne Twister Pseudo Random Number Generator
IMPLICIT NONE
!___Dummy Arguments:
INTEGER, INTENT(INOUT) :: X(:)
INTEGER, INTENT(IN)    :: N
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     SUBROUTINE permut
! DESCRIPTION:
!     Produce a random permutation of the numbers
!     in the array x(n), overwriting the original array.
! LANGUAGE:
!     Fortran 90/95
! AUTHOR:
!     Dr. Paul W. Mielke, Jr., Dept of Statistics, Colorado State University
!        http://www.stat.colostate.edu/~mielke/mielkesoftware.html
!     Modifications:
!        Jon D. Richards, Operations Research Analyst    jon_richards@usgs.gov
!        Fort Collins Science Center                  http://www.fort.usgs.gov
!        U.S. Geological Survey  -  Biological Resources Division
! INVOKED BY:
!     htqm1least <File: "regrsmod.f90: htqm1least"> module
! INVOKES:
!     -

!___Local Variables:
REAL (KIND(0.0)) :: r_Num
INTEGER :: i, j, k
INTEGER :: Nm1
INTEGER :: z
!___Intrinsic Procedures:
INTRINSIC :: INT, RANDOM_NUMBER
!___Executable Statements:
Nm1 = N - 1
DO i=1,Nm1
   j = N - i + 1
      r_Num = genrand_real3( )
   k = INT(j*r_Num) + 1
   IF (k>j) k = j
   IF (k>N) k = j ! don't exceed range
   z    = X(j)
   X(j) = X(k)
   X(k) = z
   call rchkusr()
END DO
END SUBROUTINE ipermut
!==============================================================================

SUBROUTINE permut(X, N)
!___Imported Parameters and Variables:

!___Imported Procedures:
USE mt19937, ONLY: genrand_real3
        ! ^-- Mersenne Twister Pseudo Random Number Generator
IMPLICIT NONE
!___Dummy Arguments:
REAL (KIND(0D0)), INTENT(INOUT) :: X(:)
INTEGER,          INTENT(IN)    :: N
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     SUBROUTINE permut
! DESCRIPTION:
!     Produce a random permutation of the numbers
!     in the array x(n), overwriting the original array.
! LANGUAGE:
!     Fortran 90/95
! AUTHOR:                                                                                                                                     f
!     Dr. Paul W. Mielke, Jr., Dept of Statistics, Colorado State University
!        http://www.stat.colostate.edu/~mielke/mielkesoftware.html
!     Modifications:
!        Jon D. Richards, Operations Research Analyst    jon_richards@usgs.gov
!        Fort Collins Science Center                  http://www.fort.usgs.gov
!        U.S. Geological Survey  -  Biological Resources Division
! INVOKED BY:
!     htqm1least  <File: "regrsmod.f90: htqm1least "> module
!     ftlqleast   <File: "regrsmod.f90: ftlqleast  "> module
!     htlqleast   <File: "regrsmod.f90: htlqleast  "> module
!     htqm1least  <File: "regrsmod.f90: htqm1least "> module
!     htrsleast   <File: "regrsmod.f90: htrsleast  "> module
!     htrsdpleast <File: "regrsmod.f90: htrsdpleast"> module
!     htdpleast   <File: "regrsmod.f90: htdpleast  "> module
! INVOKES:
!     -
! FILES:
!     -
! DEVICES:
!     -

!___Local Variables:
REAL (KIND(0.0)) :: r_Num
REAL (KIND(0D0)) :: z
INTEGER :: i, j, k
INTEGER :: Nm1
!___Intrinsic Procedures:
INTRINSIC :: INT, RANDOM_NUMBER
!___Executable Statements:
Nm1 = N - 1
DO i=1,Nm1
   j = N - i + 1

      r_Num = genrand_real3( )

   k = INT(j*r_Num) + 1
   IF (k>j) k = j
   IF (k>N) k = j ! don't exceed range
   z    = X(j)
   X(j) = X(k)
   X(k) = z
   call rchkusr()
END DO
END SUBROUTINE permut

!==============================================================================

SUBROUTINE L1(i_NumObs, i_NumVars, i_NumObsP2, i_NumVarsP2, da_Data,          &
      da_B_DVVals, d_Toler, da_X_Betas, da_E_Resids, ia_S_Work, d_Theta,      &
      l_TrueLad, da_DualSoln)
           !* <A NAME="L1">
!d USE debugmod, ONLY: MyDebug                                         ! debug!
!___Imported Parameters and Variables:
USE BLCMNMOD, ONLY: d_ONE, d_TWO, d_ZERO
        ! ^-- Some global Blossom variables and parameters.
USE MROLAMOD, ONLY: i_NON_UNIQUE_SOLN, i_ROUNDING_ERRORS, i_SUCCESSFUL_SOLN
        ! ^-- Permutation, regression and other Blossom stat parameters.
!___Imported Procedures:
!__(none)__
IMPLICIT NONE
!___Dummy Arguments:
INTEGER,          INTENT(IN)    :: i_NumObs ! number observations
INTEGER,          INTENT(IN)    :: i_NumVars ! number variables
INTEGER,          INTENT(IN)    :: i_NumObsP2
INTEGER,          INTENT(IN)    :: i_NumVarsP2
REAL (KIND(0D0)), INTENT(INOUT) :: da_Data(:,:) ! DATA dim(i_NumObsP2,i_NumVarsP2)
REAL (KIND(0D0)), INTENT(INOUT) :: da_B_DVVals(:) ! DEPVALS(i_NumVars)
REAL (KIND(0D0)), INTENT(IN)    :: d_Toler ! How close to 0
REAL (KIND(0D0)), INTENT(OUT)   :: da_X_Betas(:) ! BETAS (i_NumVars)
REAL (KIND(0D0)), INTENT(OUT)   :: da_E_Resids(:) ! RESIDS(i_NumObs)
INTEGER,          INTENT(OUT)   :: ia_S_Work(:) ! work space (i_NumObs)
REAL (KIND(0D0)), INTENT(IN)    :: d_Theta  ! Quantile
LOGICAL,          INTENT(IN)    :: l_TrueLad ! do a LAD not Quant
  ! da_DualSoln is the Dual Solution
REAL (KIND(0D0)), INTENT(OUT), OPTIONAL :: da_DualSoln(i_NumObs)

!___Local Variables:
      INTEGER :: I, J, K, L
      INTEGER :: i_In, i_Kount, i_Out, i_KD, i_KL, i_KR
      INTEGER :: i_NumObsP1, i_NumVarsP1
      REAL (KIND(0D0)) :: d_D, d_Big, d_Max, d_Min, d_Pivot, d_Sum, d_Wgt
      LOGICAL :: l_Stage, l_Test
!___Intrinsic Procedures:
      INTRINSIC ABS, DBLE, HUGE, PRESENT, SIGN !x. , SUM
!___Executable Statements:

      d_Big = HUGE(1.0D36)  ! d_Big must be set equal to a large real constant
        ! Initialization
      i_NumObsP1  = i_NumObs  + 1
      i_NumVarsP1 = i_NumVars + 1

      DO 10 J=1,i_NumVars
         da_Data(i_NumObsP2,J) = DBLE(J)
  10  CONTINUE
      da_X_Betas(1:i_NumVars) = d_ZERO
      DO 40 I=1,i_NumObs
         da_Data(I,i_NumVarsP2) = DBLE(i_NumVars+I)
         da_Data(I,i_NumVarsP1) = da_B_DVVals(I)
         IF (da_B_DVVals(I) >= d_ZERO) GOTO 30
         DO 20 J=1,i_NumVarsP2
             da_Data(I,J) = -da_Data(I,J)
      20 CONTINUE

 30      CONTINUE
         da_E_Resids(I) = d_ZERO
 40   CONTINUE

        ! Compute the marginal costs.
      DO 60 J=1,i_NumVarsP1
         d_Sum = d_ZERO
         DO 50 I=1,i_NumObs
            IF (l_TrueLad) THEN
               d_Sum = d_Sum + da_Data(I,J)
            ELSE
        ! Here is the part for quantile regression.
        ! NOTE: We can't use these weighted sums.
               d_Wgt = SIGN(d_ONE,da_Data(I,i_NumVarsP2))
               d_Sum = d_Sum +            &
                     da_Data(I,J)*(d_TWO*d_Theta*d_Wgt+d_ONE - d_Wgt)
            END IF
 50      CONTINUE
         da_Data(i_NumObsP1,J) = d_Sum
 60   CONTINUE

        ! Stage 1.
        ! Determine the vector to enter the basis.
      l_Stage = .TRUE.
      i_Kount = 0
      i_KR = 1
      i_KL = 1
 70   CONTINUE
      d_Max = -d_ONE
      DO 80 J=i_KR,i_NumVars
         IF (ABS(da_Data(i_NumObsP2,J)) > i_NumVars) GOTO 80
         d_D = ABS(da_Data(i_NumObsP1,J))
         IF (d_D <= d_Max) GOTO 80
         d_Max = d_D
         i_In  = J
 80   CONTINUE
        ! IF (da_Data(i_NumObsP1,i_In) >= d_ZERO) GOTO 100
      IF (da_Data(i_NumObsP1,i_In) < d_ZERO) THEN
         DO 90 I=1,i_NumObsP2
            da_Data(I,i_In) = -da_Data(I,i_In)
      90 CONTINUE
      END IF
        ! Determine the vector to leave the basis.
 100  CONTINUE
      K = 0
      DO 110 I=i_KL,i_NumObs
         d_D = da_Data(I,i_In)
         IF (d_D <= d_Toler) GOTO 110
         K  = K + 1
         da_B_DVVals(K) = da_Data(I,i_NumVarsP1)/d_D
         ia_S_Work(K)   = I
         l_Test         = .TRUE.
 110  CONTINUE
 120  CONTINUE
      IF (K > 0) GOTO 130
      l_Test = .FALSE.
      GOTO 150
 130  CONTINUE
      d_Min = d_Big
      DO 140 I=1,K
         IF (da_B_DVVals(I) >= d_Min) GOTO 140
         J = I
         d_Min = da_B_DVVals(I)
         i_Out = ia_S_Work(I)
 140  CONTINUE
      da_B_DVVals(J) = da_B_DVVals(K)
      ia_S_Work(J)   = ia_S_Work(K)
      K = K - 1
        ! Check for linear dependence in Stage I.
 150  CONTINUE
      IF (l_Test  .OR.  .NOT.l_Stage) GOTO 170

      DO 160 I=1,i_NumObsP2
         d_D = da_Data(I,i_KR)

         da_Data(I,i_KR) = da_Data(I,i_In)
         da_Data(I,i_In) = d_D
 160  CONTINUE
      i_KR = i_KR + 1
      GOTO 260
 170  CONTINUE
      IF (l_Test) GOTO 180
      !  da_Data(i_NumObsP2,i_NumVarsP1)=d_TWO ! EXIT CODE=2; Rounding Errs
      da_Data(i_NumObsP2,i_NumVarsP1) = DBLE(i_ROUNDING_ERRORS) ! EXCODE=2; Rounding Errs
      GOTO 350
 180  CONTINUE
      d_Pivot = da_Data(i_Out,i_In)
      IF (da_Data(i_NumObsP1,i_In)-d_Pivot-d_Pivot <= d_Toler) GOTO 200
      DO 190 J=i_KR,i_NumVarsP1
         d_D = da_Data(i_Out,J)
         da_Data(i_NumObsP1,J) = da_Data(i_NumObsP1,J) - d_D - d_D
         da_Data(i_Out,J) = -d_D
 190  CONTINUE
      da_Data(i_Out,i_NumVarsP2) = -da_Data(i_Out,i_NumVarsP2)
      GOTO 120
        ! Pivot on da_Data(i_Out,i_In).
 200  DO 210 J=i_KR,i_NumVarsP1
         IF (J == i_In) GOTO 210
         da_Data(i_Out,J) = da_Data(i_Out,J)/d_Pivot
 210  CONTINUE
      DO 230 I=1,i_NumObsP1
         IF (I == i_Out) GOTO 230
         d_D = da_Data(I,i_In)
         DO 220 J=i_KR,i_NumVarsP1
            IF (J == i_In) GOTO 220
            da_Data(I,J) = da_Data(I,J) - d_D*da_Data(i_Out,J)
 220     CONTINUE
 230  CONTINUE
      DO 240 I=1,i_NumObsP1
         IF (I == i_Out) GOTO 240
         da_Data(I,i_In) = -da_Data(I,i_In)/d_Pivot
 240  CONTINUE
      da_Data(i_Out,i_In) = d_ONE/d_Pivot
      d_D = da_Data(i_Out,i_NumVarsP2)
      da_Data(i_Out,i_NumVarsP2) = da_Data(i_NumObsP2,i_In)
      da_Data(i_NumObsP2,i_In) = d_D
      i_Kount = i_Kount + 1
      IF (.NOT.l_Stage) GOTO 270
        ! Interchange rows in Stage I.
      i_KL = i_KL+1
      DO 250 J=i_KR,i_NumVarsP2
         d_D = da_Data(i_Out,J)
         da_Data(i_Out,J) = da_Data(i_Kount,J)
         da_Data(i_Kount,J) = d_D
 250  CONTINUE
 260  CONTINUE
      IF (i_Kount+i_KR /= i_NumVarsP1) GOTO 70
        ! Stage II
      l_Stage = .FALSE.
        ! Determine the vector to enter the basis.
 270  CONTINUE
      d_Max = -d_Big
      DO 290 J=i_KR,i_NumVars
         d_D = da_Data(i_NumObsP1,J)
         IF (d_D >= d_ZERO)   GOTO 280
         IF (d_D > (-d_TWO)) GOTO 290
         d_D = -d_D - d_TWO
 280     CONTINUE
         IF (d_D <= d_Max) GOTO 290
         d_Max = d_D
         i_In  = J
 290  CONTINUE
      IF (d_Max <= d_Toler) GOTO 310
      IF (da_Data(i_NumObsP1,i_In) > d_ZERO) GOTO 100
         DO 300 I=1,i_NumObsP2
            da_Data(I,i_In) = -da_Data(I,i_In)
     300 CONTINUE
      da_Data(i_NumObsP1,i_In)   = da_Data(i_NumObsP1,i_In) - d_TWO
      GOTO 100
        ! Find the dual solution.
        ! From Koenker's RQD.FOR program, as interpreted to fit
        !   into our Barrodale & Roberts algorithm which solves
        !   for only one quantile (as we amended it).
        ! N.B., For our purposes we only need this for the first
        !   time through for the reduced model. - JDR  25 Feb 1999
 310  CONTINUE
      IF (PRESENT(da_DualSoln)) THEN  ! Do only if it is in argument list.
!d          CALL MyDebug("   DO I=1,i_NumVars")                        ! debug!
         DO I=1,i_NumVars
            i_KD = ABS(da_Data(i_NumObsP2,I)) - i_NumVars
            da_DualSoln(i_KD) = d_ONE + da_Data(i_NumObsP1,I)/d_TWO
            IF (da_Data(i_NumObsP2,I) < d_ZERO) THEN
               da_DualSoln(i_KD) = d_ONE - da_DualSoln(i_KD)
            END IF

         END DO

         DO I=i_KL,i_NumObs
            i_KD = ABS(da_Data(I,i_NumVarsP2)) - i_NumVars
            IF (da_Data(I,i_NumVarsP2) < d_ZERO) da_DualSoln(i_KD) = d_ZERO
            IF (da_Data(I,i_NumVarsP2) > d_ZERO) da_DualSoln(i_KD) = d_ONE

         END DO
      END IF
        ! Prepare the output.
      L = i_KL - 1
      DO 330 I=1,L
        ! IF (da_Data(I,i_NumVarsP1) >= d_ZERO) GOTO 330
         IF (da_Data(I,i_NumVarsP1) < d_ZERO) THEN
            DO 320 J=i_KR,i_NumVarsP2
               da_Data(I,J) = -da_Data(I,J)
        320 CONTINUE
         END IF
 330  CONTINUE
      !  da_Data(i_NumObsP2,i_NumVarsP1) = d_ZERO ! EXIT CODE=0; Non-Unique Solution
      da_Data(i_NumObsP2,i_NumVarsP1) = DBLE(i_NON_UNIQUE_SOLN) ! EXCODE=0; Non-Unique
      IF (i_KR /= 1) GOTO 350
      DO 340 J=1,i_NumVars
         d_D = ABS(da_Data(i_NumObsP1,J))
         IF ((d_D <= d_Toler)  .OR.  (d_TWO - d_D <= d_Toler)) GOTO 350
 340  CONTINUE
      !  da_Data(i_NumObsP2,i_NumVarsP1) = d_ONE ! EXIT CODE=1; Successful(Unique)
      da_Data(i_NumObsP2,i_NumVarsP1) = DBLE(i_SUCCESSFUL_SOLN) ! EXCODE=1; Unique
 350  CONTINUE
      DO 380 I=1,i_NumObs

         K  = da_Data(I,i_NumVarsP2)
         d_D = da_Data(I,i_NumVarsP1)

         IF (K > 0) GOTO 360
         K  = -K
         d_D = -d_D
 360     CONTINUE
         IF (I >= i_KL) GOTO 370

         da_X_Betas(K) = d_D
         GOTO 380
 370     CONTINUE
         K  = K - i_NumVars
         da_E_Resids(K) = d_D

 380  CONTINUE
      da_Data(i_NumObsP2,i_NumVarsP2) = i_Kount
      da_Data(i_NumObsP1,i_NumVarsP2) = i_NumVarsP1 - i_KR
      d_Sum = d_ZERO
      DO 390 I=i_KL,i_NumObs
         d_Sum = d_Sum + da_Data(I,i_NumVarsP1)
  390 CONTINUE
      da_Data(i_NumObsP1,i_NumVarsP1) = d_Sum
!d       CALL MyDebug("<< outa L1")                                    ! debug!
call rchkusr()
      RETURN   ! ... normal exit ...
END SUBROUTINE L1

!==============================================================================

SUBROUTINE lsqleast(i_NumObs, i_NumVars, i_NumPermut, i_Seed, i_NumToDrop,    &
      i_FullTest, ia_DropPos, da_ADataLSQ, da_X, da_XRed, d_R, d_RR,          &
      d_SSRRed, d_SSR, d_MeanSqError, da_YH, da_E, i_CntStat, d_PValueFull,   &
      d_PValueReduced, d_To, l_ComputePearsonR, l_DoLadSave, iEr,ch_MyMsg)
!d USE debugmod, ONLY: MyDebug                                         ! debug!
!___Imported Parameters and Variables:
USE blcmnmod, ONLY: d_ONE, d_ZERO, i_OK
        ! ^-- Some global Blossom variables and parameters.
USE jonsmodule, ONLY: i_CCode, i_REGSRMOD_ER008, i_REGSRMOD_ER009, &
      i_REGSRMOD_ER011
        ! ^-- Message handling, etc.
USE mrolamod, ONLY: i_DO_FULL_TEST
!___Imported Procedures:
USE invertmod, ONLY: invert
        ! ^__ invert: matrix inversion
USE jonsmodule, ONLY: lf_Equals
        ! ^-- lf_Equals: Generic test for equality of two objects.
USE mt19937, ONLY: init_genrand
        ! ^-- Mersenne Twister Pseudo Random Number Generator
IMPLICIT NONE
!___Dummy Arguments:
INTEGER,          INTENT(IN)    :: i_NumObs
INTEGER,          INTENT(IN)    :: i_NumVars
INTEGER,          INTENT(IN)    :: i_NumPermut
INTEGER,          INTENT(IN)    :: i_Seed
INTEGER,          INTENT(IN)    :: i_NumToDrop
INTEGER,          INTENT(IN)    :: i_FullTest
INTEGER,          INTENT(IN)    :: ia_DropPos(:)
REAL (KIND(0D0)), INTENT(IN)    :: da_ADataLSQ(:,:)
REAL (KIND(0D0)), INTENT(OUT)   :: da_X(:)
REAL (KIND(0D0)), INTENT(OUT)   :: da_XRed(:)
REAL (KIND(0D0)), INTENT(OUT)   :: d_R
REAL (KIND(0D0)), INTENT(OUT)   :: d_RR
REAL (KIND(0D0)), INTENT(OUT)   :: d_SSRRed
REAL (KIND(0D0)), INTENT(OUT)   :: d_SSR
REAL (KIND(0D0)), INTENT(OUT)   :: d_MeanSqError
REAL (KIND(0D0)), INTENT(OUT)   :: da_YH(:)
REAL (KIND(0D0)), INTENT(OUT)   :: da_E(:)
INTEGER,          INTENT(OUT)   :: i_CntStat
REAL (KIND(0D0)), INTENT(OUT)   :: d_PValueFull
REAL (KIND(0D0)), INTENT(OUT)   :: d_PValueReduced
REAL (KIND(0D0)), INTENT(OUT)   :: d_To
LOGICAL,          INTENT(INOUT) :: l_ComputePearsonR
LOGICAL,          INTENT(IN)    :: l_DoLadSave
INTEGER,          INTENT(OUT)   :: iEr   ! error flag
INTEGER, PARAMETER :: i_LEN_MY_EMSG = 254 ! needs to be this big for output of var list in dostatus.
CHARACTER (LEN=i_LEN_MY_EMSG), INTENT(OUT) :: ch_MyMsg
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     SUBROUTINE lsqleast
! DESCRIPTION:
!     Computes the least square regression and testing.
!     N.B.: Because of problems with numerical precision
!        when we feed a constant vector of one independent
!        variable (i.e., when there is only a constant term,
!          Y = CONSTANT
!        we shouldn't attempt to compute Pearson's Correlation
!        Coefficient "d_R".  For our purposes we are setting a
!        rule to compute "d_R" (and its square) if and only if:
!          1. First time through loop
!          2. Full model
!          3. There is more than 1 Independent Variable
!        We will use logical variable l_ComputePearsonR to flag this.
!        JDR  17 Sep 1998

!___Local Variables:
REAL (KIND(0D0)), ALLOCATABLE :: da_B(:,:)
REAL (KIND(0D0)), ALLOCATABLE :: da_BRed(:,:)
REAL (KIND(0D0)), ALLOCATABLE :: da_RedA(:,:) ! reduced data matrix
REAL (KIND(0D0)), ALLOCATABLE :: da_C(:)
REAL (KIND(0D0)), ALLOCATABLE :: da_CRed(:)
        ! Resids for reduced model, 1st time thru, not permuted (a.k.a. Y*):
REAL (KIND(0D0)), ALLOCATABLE :: da_RedResids1st(:)
REAL (KIND(0D0)), ALLOCATABLE :: da_Y(:)  ! dependent observations
REAL (KIND(0D0)), ALLOCATABLE :: da_X1st(:)  ! regression coefficients
REAL (KIND(0D0)), ALLOCATABLE :: da_XRed1st(:) ! RedModel regr coeffs
REAL (KIND(0D0)) :: d_SSR1st   ! sum squares residuals, original data, 1st time thru
REAL (KIND(0D0)) :: d_SSRRed1st
REAL (KIND(0D0)) :: d_TestStat ! test stat for permuted data (compare to d_To)
! from the lsqredcalc routine:
REAL (KIND(0D0)) :: d_CO, d_COO, d_COP, d_CP, d_CPP
REAL (KIND(0D0)) :: d_Expression
!
INTEGER :: i, j, i_JCount
INTEGER :: i_AlStat    ! status flag from dynamic array allocation
INTEGER :: i_CntPerms  ! count of number of perms
INTEGER :: i_NPerm
INTEGER :: i_NumVRed   ! number of independent variables in reduced model
INTEGER :: ios ! error status flag
INTEGER :: i_Sz        ! Fortran90 PRNG size parameter
LOGICAL :: l_ComputeR1st ! If we computed Pearson R the first time through.
LOGICAL :: l_DoFull    ! do full test? T/F
LOGICAL :: l_DoTest    ! do permutation test of regression: (full/reduced)? T/F
LOGICAL :: l_FirstTimeThru  ! first time through? T/F
LOGICAL :: l_ReducedModel   ! do reduced model & test? T/F
!___Intrinsic Procedures:
INTRINSIC :: ALLOCATED, DBLE, DOT_PRODUCT, MATMUL, RANDOM_SEED, SQRT, SUM, &
             TRANSPOSE

iEr = i_OK ! no problem yet
d_MeanSqError   = -HUGE(d_ONE) ! absurd value
d_PValueFull    = -HUGE(d_ONE) ! absurd value
d_PValueReduced = -HUGE(d_ONE) ! absurd value
d_R             = -HUGE(d_ONE) ! absurd value
d_RR            = -HUGE(d_ONE) ! absurd value
d_SSR           = -HUGE(d_ONE) ! absurd value
d_SSRRed        = -HUGE(d_ONE) ! absurd value
i_NPerm = i_NumPermut
i_CntPerms = 0
l_ReducedModel  = .FALSE.
l_DoTest        = .FALSE.
l_DoFull        = .FALSE.
l_ComputeR1st   = .FALSE.
l_FirstTimeThru = .TRUE.

IF (i_NumToDrop /= 0) THEN
   l_ReducedModel = .TRUE.
   l_DoFull       = .FALSE.
   l_DoTest       = .TRUE.
END IF
IF (i_FullTest == i_DO_FULL_TEST) THEN
   l_ReducedModel = .FALSE.
   l_DoFull       = .TRUE.
   l_DoTest       = .TRUE.
ELSE
   l_DoFull = .FALSE.
   IF (i_NumToDrop == 0) i_NPerm = 0
END IF
IF (i_NPerm /= 0) THEN  ! if i_NumPermut = 0, then no testing is done
   l_DoTest   = .TRUE.  ! we will be doing a test
   i_CntStat  = 0       ! initialize counter for statistic, full model
   i_CntPerms = 0       ! number of permutations that have run
   ! Initialize default PRNG
   CALL RANDOM_SEED(SIZE=i_Sz)
   CALL RANDOM_SEED(PUT=(/(i_Seed,i=1,i_Sz)/))
   ! Initialize Mersenne Twister PRNG
   CALL init_genrand(i_Seed)
END IF

ALLOCATE(                               &
      da_X1st(1:i_NumVars),             & ! regr coeffs 1st time thru
         da_B(1:i_NumVars,1:i_NumVars), &
         da_Y(1:i_NumObs),              & ! observed values of dependent variable
         da_C(1:i_NumVars),             &
      STAT=i_AlStat)
IF (i_AlStat /= i_OK) THEN ! Memory allocation error: da_B,da_Y,da_C in LSQLEAST
   i_CCode = i_AlStat
   iEr = i_REGSRMOD_ER008
   GOTO 90000   ! ... error exit ...
END IF
IF (l_ReducedModel) THEN        ! allocate space for reduced model arrays
   i_NumVRed = i_NumVars - i_NumToDrop ! number of independ. vars in reduced model
   ALLOCATE(                                     &
         da_XRed1st(1:i_NumVRed),                & ! regr coeffs, red model, 1st time
            da_RedA(1:i_NumObs,  1:i_NumVRed+1), & ! reduced model data
    da_RedResids1st(1:i_NumObs),                 & ! residuals, reduced model
            da_BRed(1:i_NumVRed, 1:i_NumVRed),   &
            da_CRed(1:i_NumVRed),                &
         STAT=i_AlStat)
   IF (i_AlStat /= i_OK) THEN ! Memory allocation error: da_RedA,da_RedResids1st,da_BRed,da_CRed in LSQLEAST
      i_CCode = i_AlStat
      iEr = i_REGSRMOD_ER009
      GOTO 90000   ! ... error exit ...
   END IF ! (i_AlStat /= i_OK)
END IF ! (l_ReducedModel)
! Fill a data matrix for reduced model. Remember the last
! column in da_ADataLSQ matrix are dependent variable values.
! If we have a reduced model, we will drop some variables.
IF (l_ReducedModel) THEN     ! REDUCED model:
   i_JCount = 1
   DO j=1,i_NumVars+1
      IF (j == i_NumVars+1) THEN
         ! dependent varaiable:
         da_RedA(1:i_NumObs,i_JCount) = da_ADataLSQ(1:i_NumObs,j)
      ELSE
         IF (ia_DropPos(j) == 0) THEN
            ! independent variables:
            da_RedA(1:i_NumObs,i_JCount) = da_ADataLSQ(1:i_NumObs,j)
            i_JCount = i_JCount + 1
         END IF
      END IF ! ELSE (J /= n+1)
   END DO ! J
END IF ! (l_ReducedModel)
! ------------------- main LSQ permutation loop ----------
MainPermLoop : DO
   IF (l_FirstTimeThru) THEN
      ! Compute Pearson's correlation coefficient if and only if:
      !   1. First time through loop
      !   2. Full model
      !   3. There is > 1 Independent Variable
      IF (l_DoFull  .AND.  (i_NumVars > 1)) THEN
         l_ComputePearsonR = .TRUE.
         l_ComputeR1st     = l_ComputePearsonR
      ELSE
         l_ComputePearsonR = .FALSE.
      END IF
      da_Y(1:i_NumObs) = da_ADataLSQ(1:i_NumObs,i_NumVars+1)
   ELSE ! Second and subsequent passes through this loop, do PERMUTATIONS:
      ! We don't need Pearson's correlation coefficient.
      l_ComputePearsonR = .FALSE.
      ! If this is a REDUCED model test then permute the residuals vector
      ! and use this in the next regression in place of the observed
      ! dependent variable values.
      IF (l_ReducedModel) THEN
         CALL permut(da_RedResids1st, i_NumObs)
         da_Y(1:i_NumObs) = da_RedResids1st(1:i_NumObs)
         ! Else if this is a FULL model test, then permute the observed
         ! dependent variable values.
      ELSE
         CALL permut(da_Y, i_NumObs)
      END IF
   END IF ! End of PERMUTATIONS for 2nd and subsequent passes through loop.
            ! ----v----- BEGIN calculate OLSQ resgression items ----v----- !
   ! If we are testing reduced model, we want to compare results of
   ! regression of full model (with permuted Y*) with regression of
   ! reduced model (with same Y*)
   !r CALL lsqredcalc(l_ComputePearsonR, i_NumVars, i_NumObs, da_ADataLSQ, da_Y, &
   !r da_C, da_B, da_X, da_YH, da_E, d_SSR, d_R, d_RR, iEr)
   da_C(1:i_NumVars) = MATMUL(da_Y(1:i_NumObs), da_ADataLSQ(1:i_NumObs,1:i_NumVars))
   da_B(1:i_NumVars,1:i_NumVars) = MATMUL(TRANSPOSE(da_ADataLSQ(1:i_NumObs,1:i_NumVars)),  &
                                                    da_ADataLSQ(1:i_NumObs,1:i_NumVars))
   CALL invert(i_NumVars, da_B(1:i_NumVars,1:i_NumVars), iEr)
   IF (iEr /= i_OK) THEN   ! in the case of a singular matrix, can't invert
      GOTO 90000   ! ... error exit ...
   END IF
   da_X(1:i_NumVars) = MATMUL(da_C(1:i_NumVars), da_B(1:i_NumVars,1:i_NumVars))

   da_YH(1:i_NumObs) = MATMUL(da_X(1:i_NumVars), &
            TRANSPOSE(da_ADataLSQ(1:i_NumObs,1:i_NumVars)))
   da_E(1:i_NumObs) = da_Y(1:i_NumObs) - da_YH(1:i_NumObs)
   IF (l_ComputePearsonR) THEN
      d_CO  = SUM(da_Y(1:i_NumObs))
      d_COO = SUM(da_Y(1:i_NumObs)*da_Y(1:i_NumObs))
      d_CP  = SUM(da_YH(1:i_NumObs))
      d_CPP = SUM(da_YH(1:i_NumObs)*da_YH(1:i_NumObs))
      d_COP = SUM(da_Y(1:i_NumObs)*da_YH(1:i_NumObs))
   END IF
   ! Because of numerical precision problems, we can have with an
   ! expression with a negative value here.  We can't take the
   ! square root of this. This happens when computing R with the
   ! permuted data, and we don't need R anyway for this, so we
   ! will skip it.  The statements below can trap this. In fact,
   ! the data captured here is reasonable data, and could exist
   ! in reality, so we should expect some data to create problems
   ! with this program, even non-permuted data. - JDR   8 Feb 1999
   IF (l_ComputePearsonR) THEN
      ! Pearson correlation coefficient:
      d_Expression = (d_COO*DBLE(i_NumObs)-d_CO*d_CO) * &
                     (d_CPP*DBLE(i_NumObs)-d_CP*d_CP)
      IF (d_Expression <= d_ZERO  .OR.  lf_Equals(d_Expression, d_ZERO)) THEN
         d_R = d_ZERO
      ELSE
         d_R = (d_COP*DBLE(i_NumObs)-d_CO*d_CP) / SQRT(d_Expression)
      END IF
      d_RR = d_R * d_R       ! square of Pearson correlation coefficient
   END IF
   d_SSR = DOT_PRODUCT(da_E(1:i_NumObs), da_E(1:i_NumObs))
   !
   IF (l_FirstTimeThru) THEN  ! Save unpermuted values of some results.
      da_X1st(1:i_NumVars) = da_X(1:i_NumVars)   ! (Array assignment)
      d_SSR1st = d_SSR  ! sum squares residuals of the original data

   END IF

   IF (l_ReducedModel) THEN
      !r       CALL lsqredcalc(l_ComputePearsonR, i_NumVRed, i_NumObs, da_RedA, da_Y,  &
      !r             da_CRed, da_BRed, da_XRed, da_YH, da_E, d_SSRRed, d_R, d_RR, iEr)
      da_CRed(1:i_NumVRed) = MATMUL(da_Y(1:i_NumObs), da_RedA(1:i_NumObs,1:i_NumVRed))
      da_BRed(1:i_NumVRed,1:i_NumVRed) = MATMUL(TRANSPOSE(da_RedA(1:i_NumObs,1:i_NumVRed)), &
                                 da_RedA(1:i_NumObs,1:i_NumVRed))
      CALL invert(i_NumVRed, da_BRed(1:i_NumVRed,1:i_NumVRed), iEr)
      IF (iEr /= i_OK) THEN   ! in the case of a singular matrix, can't invert
         GOTO 90000   ! ... error exit ...
      END IF
      da_XRed(1:i_NumVRed) = MATMUL(da_CRed(1:i_NumVRed), da_BRed(1:i_NumVRed,1:i_NumVRed))
      da_YH(1:i_NumObs) = MATMUL(da_XRed(1:i_NumVRed), &
               TRANSPOSE(da_RedA(1:i_NumObs,1:i_NumVRed)))
      da_E(1:i_NumObs) = da_Y(1:i_NumObs) - da_YH(1:i_NumObs)

      d_SSRRed = DOT_PRODUCT(da_E(1:i_NumObs), da_E(1:i_NumObs))

      IF (l_FirstTimeThru) THEN  ! Save unpermuted values of some results.
         da_XRed1st(1:i_NumVRed) = da_XRed(1:i_NumVRed)
         d_SSRRed1st = d_SSRRed ! sum squares resids of the original red model

      END IF
   END IF
            ! ----^----- END calculate OLSQ resgression items ----^----- !
   IF (l_FirstTimeThru) THEN
      IF (l_DoTest) THEN
        ! 1st time through, the regression itself satisfies
        ! the condition TestStat >= TObserved
         i_CntStat = 1
      END IF
      d_MeanSqError = SQRT(d_SSR)/DBLE(i_NumObs)

      IF (l_ReducedModel) THEN
         da_RedResids1st(1:i_NumObs) = da_E(1:i_NumObs)

         d_To = (d_SSRRed-d_SSR) / d_SSR     ! observed test statistic
      END IF
      ! HERE we want to do the /SAVE option:

   END IF   ! (l_FirstTimeThru)
   IF (l_FirstTimeThru) THEN
      l_FirstTimeThru = .FALSE.
   ELSE
      IF (l_DoTest) THEN  ! TESTING: make counts
         i_CntPerms = i_CntPerms + 1
         IF (l_DoFull) THEN         ! (FULL model)
            IF (i_CntPerms <= i_NumPermut) THEN
               IF (d_SSR <= d_SSR1st) THEN  ! sum sq resids <= original SSR?
                  i_CntStat = i_CntStat + 1
               END IF
            END IF
         ELSE                      ! (REDUCED model)
            IF (i_CntPerms <= i_NumPermut) THEN
               d_TestStat = (d_SSRRed-d_SSR) / d_SSR
               IF (d_TestStat >= d_To) THEN ! permuted test stat >= obs test stat?
                  i_CntStat = i_CntStat + 1
               END IF
            END IF
         END IF
      END IF ! End of TESTING: make counts
   END IF
   IF (i_CntPerms >= i_NumPermut) EXIT MainPermLoop ! exit the MainPermLoop: done
   IF (.NOT.l_DoTest) EXIT MainPermLoop
   call rchkusr()
END DO MainPermLoop
! ------------------- END main LSQ permutation loop --------
! P-Values of tests are these simple proportions:
IF (l_DoFull) THEN
   d_PValueFull = DBLE(i_CntStat)/DBLE(i_NumPermut)
ELSE IF (l_ReducedModel) THEN

   d_PValueReduced = DBLE(i_CntStat)/DBLE(i_NumPermut)
END IF
90000 CONTINUE
IF (ALLOCATED(da_B)) DEALLOCATE(da_B, STAT=ios)
IF (ALLOCATED(da_Y)) DEALLOCATE(da_Y, STAT=ios)
IF (ALLOCATED(da_C)) DEALLOCATE(da_C, STAT=ios)
IF (l_ReducedModel) THEN
   IF (ALLOCATED(da_RedA        )) DEALLOCATE(da_RedA,         STAT=ios)
   IF (ALLOCATED(da_RedResids1st)) DEALLOCATE(da_RedResids1st, STAT=ios)
   IF (ALLOCATED(da_BRed        )) DEALLOCATE(da_BRed,         STAT=ios)
   IF (ALLOCATED(da_CRed        )) DEALLOCATE(da_CRed,         STAT=ios)
END IF
! Return original (unpermuted) values to calling procedure:
IF (ALLOCATED(da_X1st)) THEN
   da_X(1:i_NumVars) = da_X1st(1:i_NumVars)
   d_SSR = d_SSR1st
   DEALLOCATE(da_X1st, STAT=ios)
ELSE
   da_X = -HUGE(d_ONE) ! absurd value
   d_SSR = -HUGE(d_ONE) ! absurd value
END IF
IF (ALLOCATED(da_XRed1st)) THEN
   da_XRed(1:i_NumVRed) = da_XRed1st(1:i_NumVRed)
   d_SSRRed = d_SSRRed1st
   DEALLOCATE(da_XRed1st, STAT=ios)
ELSE
   da_XRed = -HUGE(d_ONE) ! absurd value
   d_SSRRed = -HUGE(d_ONE) ! absurd value
END IF
l_ComputePearsonR = l_ComputeR1st
RETURN   ! ... normal exit ...
END SUBROUTINE lsqleast

!==============================================================================

SUBROUTINE lsqleastdp(i_NumObs, i_NumVars, i_NumPermut, i_Seed, i_NumToDrop, &
      ia_DropPos, da_ADataLSQ, da_X, da_XRed, d_SSRRed, d_SSR, &
      d_MeanSqError, da_YH, da_E, i_CntStat, d_PValueReduced, d_To, iEr,ch_MyMsg)
!d USE debugmod, ONLY: MyDebug                                         ! debug!
!___Imported Parameters and Variables:
USE blcmnmod, ONLY: d_ZERO, d_ONE, d_HALF, i_OK
        ! ^-- Some global Blossom variables and parameters.
USE jonsmodule, ONLY: i_CCode, i_REGSRMOD_ER008, i_REGSRMOD_ER009, &
      i_REGSRMOD_ER011
        ! ^-- Message handling, etc.
!___Imported Procedures:
USE binomod, ONLY: random_binomial
        ! ^-- random_binomial: random binomial deviate
USE invertmod, ONLY: invert
        ! ^__ invert: matrix inversion
USE jonsmodule, ONLY: lf_Equals
        ! ^-- lf_Equals: Generic test for equality of two objects.
USE mt19937, ONLY: init_genrand
        ! ^-- Mersenne Twister Pseudo Random Number Generator
IMPLICIT NONE
!___Dummy Arguments:
INTEGER,          INTENT(IN)  :: i_NumObs
INTEGER,          INTENT(IN)  :: i_NumVars
INTEGER,          INTENT(IN)  :: i_NumPermut
INTEGER,          INTENT(IN)  :: i_Seed
INTEGER,          INTENT(IN)  :: i_NumToDrop
INTEGER,          INTENT(IN)  :: ia_DropPos(:)
REAL (KIND(0D0)), INTENT(IN)  :: da_ADataLSQ(:,:)
REAL (KIND(0D0)), INTENT(OUT) :: da_X(:)
REAL (KIND(0D0)), INTENT(OUT) :: da_XRed(:)
REAL (KIND(0D0)), INTENT(OUT) :: d_SSRRed
REAL (KIND(0D0)), INTENT(OUT) :: d_SSR
REAL (KIND(0D0)), INTENT(OUT) :: d_MeanSqError
REAL (KIND(0D0)), INTENT(OUT) :: da_YH(:)
REAL (KIND(0D0)), INTENT(OUT) :: da_E(:)
INTEGER,          INTENT(OUT) :: i_CntStat
REAL (KIND(0D0)), INTENT(OUT) :: d_PValueReduced
REAL (KIND(0D0)), INTENT(OUT) :: d_To
INTEGER,          INTENT(OUT) :: iEr   ! error flag
INTEGER, PARAMETER :: i_LEN_MY_EMSG = 254 ! needs to be this big for output of var list in dostatus.
CHARACTER (LEN=i_LEN_MY_EMSG), INTENT(OUT) :: ch_MyMsg
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     SUBROUTINE lsqleastdp
! DESCRIPTION:
!     Computes the least square regression testing with double permutation.
!  .....
!  HYP (ols) /DP..................................................(HYP_OLS_DP)
!     Condition:  1. OLS was first command.
!                 2. HYP command with reduced model.
!                 3. Option /DP (double permutation)
!
!        Action:  Permute residuals from reduced model (null Ho) original,
!                 with double permutation (adjusted signs).
!
!        Output:  Betas,
!                 Sum of squares of the residuals,
!                 Observed Test Statistic,
!                 P-value of variables in full model but not in reduced
!                 model
!  .....
! LANGUAGE:
!     Fortran 90/95
! AUTHOR:
!     Dr. Paul W. Mielke, Jr., Dept of Statistics, Colorado State University
!        http://www.stat.colostate.edu/~mielke/mielkesoftware.html
!     Modifications:
!        Jon D. Richards, Operations Research Analyst    jon_richards@usgs.gov
!        Fort Collins Science Center                  http://www.fort.usgs.gov
!        U.S. Geological Survey  -  Biological Resources Division
! INVOKED BY:
!
! INVOKES:
!     permut        <File: "regrsmod.f90: permut">        module

! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!___Procedure Parameters:
! CHARACTER (LEN=*), PARAMETER :: ch_PROC_NAME = "LSQLEASTDP"
!___Local Variables:
REAL (KIND(0D0)), ALLOCATABLE :: da_B(:,:)
REAL (KIND(0D0)), ALLOCATABLE :: da_BRed(:,:)
REAL (KIND(0D0)), ALLOCATABLE :: da_RedA(:,:) ! reduced data matrix
REAL (KIND(0D0)), ALLOCATABLE :: da_C(:)
REAL (KIND(0D0)), ALLOCATABLE :: da_CRed(:)
        ! Resids for reduced model, 1st time thru, not permuted (a.k.a. Y*):
REAL (KIND(0D0)), ALLOCATABLE :: da_RedResids1st(:)
REAL (KIND(0D0)), ALLOCATABLE :: da_Y(:)  ! dependent observations
REAL (KIND(0D0)), ALLOCATABLE :: da_X1st(:)  ! regression coefficients
REAL (KIND(0D0)), ALLOCATABLE :: da_XRed1st(:) ! RedModel regr coeffs
!
REAL (KIND(0D0)), ALLOCATABLE :: da_Bin50(:)
!
REAL (KIND(0D0)) :: d_SSR1st   ! sum squares residuals, original data, 1st time thru
REAL (KIND(0D0)) :: d_SSRRed1st
REAL (KIND(0D0)) :: d_Tstar    ! test stat for permuted data (compare to d_To)
INTEGER :: i_RanBinDev         ! random binomial deviate
!
INTEGER :: i, j, i_JCount
INTEGER :: i_AlStat    ! status flag from dynamic array allocation
INTEGER :: i_CntPerms  ! count of number of perms
INTEGER :: i_NumVRed   ! number of independent variables in reduced model
INTEGER :: ios         ! error status flag
INTEGER :: i_Sz        ! Fortran90 PRNG size parameter
LOGICAL :: l_First
!___Intrinsic Procedures:
INTRINSIC :: ALLOCATED, DBLE, DOT_PRODUCT, HUGE, MATMUL, RANDOM_SEED, REAL, &
             SQRT, TRANSPOSE

iEr = i_OK ! no problem yet
d_MeanSqError    = -HUGE(d_ONE) ! absurd value
d_PValueReduced  = -HUGE(d_ONE) ! absurd value
d_SSR            = -HUGE(d_ONE) ! absurd value
d_SSRRed         = -HUGE(d_ONE) ! absurd value
d_To             = -HUGE(d_ONE) ! absurd value
i_CntStat  = 0  ! initialize counter for statistic
! Initialize default PRNG
CALL RANDOM_SEED(SIZE=i_Sz)
CALL RANDOM_SEED(PUT=(/(i_Seed,i=1,i_Sz)/))
! Initialize Mersenne Twister PRNG
CALL init_genrand(i_Seed)
ALLOCATE(                               &
      da_X1st(1:i_NumVars),             & ! regr coeffs 1st time thru
         da_B(1:i_NumVars,1:i_NumVars), &
         da_Y(1:i_NumObs),              & ! observed values of dependent variable
         da_C(1:i_NumVars),             &
      STAT=i_AlStat)
IF (i_AlStat /= i_OK) THEN ! Memory allocation error: da_B,da_Y,da_C in LSQLEASTDP
   i_CCode = i_AlStat
   !//////////////////////////////////
   iEr = i_REGSRMOD_ER008
   !//////////////////////////////////
   GOTO 90000   ! ... error exit ...
END IF
i_NumVRed = i_NumVars - i_NumToDrop ! number of independ. vars in reduced model
ALLOCATE(                                     &
      da_XRed1st(1:i_NumVRed),                & ! regr coeffs, red model, 1st time
         da_RedA(1:i_NumObs,  1:i_NumVRed+1), & ! reduced model data
 da_RedResids1st(1:i_NumObs),                 & ! residuals, reduced model 1st time
         da_BRed(1:i_NumVRed, 1:i_NumVRed),   &
         da_CRed(1:i_NumVRed),                &
        da_Bin50(1:i_NumObs), &
      STAT=i_AlStat)
IF (i_AlStat /= i_OK) THEN ! Memory allocation error: da_RedA,da_RedResids1st,da_BRed,da_CRed in LSQLEASTDP
   i_CCode = i_AlStat
   !//////////////////////////////////
   iEr = i_REGSRMOD_ER009
   !//////////////////////////////////
   GOTO 90000   ! ... error exit ...
END IF ! (i_AlStat /= i_OK)
! Fill a data matrix for reduced model. Remember the last
! column in da_ADataLSQ matrix are dependent variable values.
! If we have a reduced model, we will drop some variables.
i_JCount = 1
DO j=1,i_NumVars+1
   IF (j == i_NumVars+1) THEN
      ! dependent varaiable:
      da_RedA(1:i_NumObs,i_JCount) = da_ADataLSQ(1:i_NumObs,j) ! Array assignment
   ELSE
      IF (ia_DropPos(j) == 0) THEN
         ! independent variables:
         da_RedA(1:i_NumObs,i_JCount) = da_ADataLSQ(1:i_NumObs,j) ! Array assignment
         i_JCount = i_JCount + 1
      END IF
   END IF
   call rchkusr()
END DO
! Do LSQ with Full Model...
da_Y(1:i_NumObs) = da_ADataLSQ(1:i_NumObs,i_NumVars+1) ! Array assignment
da_C(1:i_NumVars) = MATMUL(da_Y(1:i_NumObs), da_ADataLSQ(1:i_NumObs,1:i_NumVars))
da_B(1:i_NumVars,1:i_NumVars) = MATMUL(TRANSPOSE(da_ADataLSQ(1:i_NumObs,1:i_NumVars)),  &
                        da_ADataLSQ(1:i_NumObs,1:i_NumVars))
CALL invert(i_NumVars, da_B(1:i_NumVars,1:i_NumVars), iEr)
IF (iEr /= i_OK) THEN   ! in the case of a singular matrix, can't invert
   GOTO 90000   ! ... error exit ...
END IF
da_X(1:i_NumVars) = MATMUL(da_C(1:i_NumVars), da_B(1:i_NumVars,1:i_NumVars))
da_YH(1:i_NumObs) = MATMUL(da_X(1:i_NumVars), &
         TRANSPOSE(da_ADataLSQ(1:i_NumObs,1:i_NumVars)))
da_E(1:i_NumObs) = da_Y(1:i_NumObs) - da_YH(1:i_NumObs)
d_SSR = DOT_PRODUCT(da_E(1:i_NumObs), da_E(1:i_NumObs))
d_MeanSqError = SQRT(d_SSR)/DBLE(i_NumObs) ! In case we ever want to see this
da_X1st(1:i_NumVars) = da_X(1:i_NumVars)
d_SSR1st = d_SSR  ! sum squares residuals of the original data
! ... Then Do LSQ with Reduced Model
da_CRed(1:i_NumVRed) = MATMUL(da_Y(1:i_NumObs), da_RedA(1:i_NumObs,1:i_NumVRed))
da_BRed(1:i_NumVRed,1:i_NumVRed) = MATMUL(TRANSPOSE(da_RedA(1:i_NumObs,1:i_NumVRed)), &
                           da_RedA(1:i_NumObs,1:i_NumVRed))
CALL invert(i_NumVRed, da_BRed(1:i_NumVRed,1:i_NumVRed), iEr)
IF (iEr /= i_OK) THEN   ! in the case of a singular matrix, can't invert
   GOTO 90000   ! ... error exit ...
END IF
da_XRed(1:i_NumVRed) = MATMUL(da_CRed(1:i_NumVRed), da_BRed(1:i_NumVRed,1:i_NumVRed))
da_YH(1:i_NumObs) = MATMUL(da_XRed(1:i_NumVRed), &
         TRANSPOSE(da_RedA(1:i_NumObs,1:i_NumVRed)))
da_E(1:i_NumObs) = da_Y(1:i_NumObs) - da_YH(1:i_NumObs)
d_SSRRed = DOT_PRODUCT(da_E(1:i_NumObs), da_E(1:i_NumObs))
da_XRed1st(1:i_NumVRed) = da_XRed(1:i_NumVRed)
d_SSRRed1st = d_SSRRed ! sum squares resids of the original reduced model
da_RedResids1st(1:i_NumObs) = da_E(1:i_NumObs)
! prior to the permutation loop.
! Compute Observed Test Statistic using SSR & SSRRed
d_To = (d_SSRRed-d_SSR) / d_SSR
! 1st time, regression itself satisfies condition TestStat >= TObserved
i_CntStat = 1
l_First = .TRUE. ! to initialize random_binomial function
!
! ------------------- main LSQ permutation loop ----------
MainPermLoop : DO i_CntPerms=2,i_NumPermut
   ! Here is where we want to put the double permutation trick.
   ! We will get a random binomial variate Nrbv based on 0.5
   ! Assign -1.0 to all of Bin50, then assign Nrbv elements to 1.0
   ! Permut the Bin50.
   ! Multiply the Residual vector da_ResRed by the Bin50 to "Center" them.
   ! Use this new "centered" residual vector in what follows (permut and assign to da_Y)
   i_RanBinDev = random_binomial(i_NumObs, REAL(d_HALF), l_First)
   da_Bin50(1:i_NumObs) = -d_ONE   ! All values are -1.0 ...
   da_Bin50(1:i_RanBinDev) = d_ONE      ! ... except this random variate number is 1.0
   CALL permut(da_Bin50, i_NumObs) ! Randomize the order of these
   da_Y(1:i_NumObs) = da_RedResids1st(1:i_NumObs) ! Reset from original resids
   da_Y(1:i_NumObs) = da_Y(1:i_NumObs) * da_Bin50(1:i_NumObs) ! "Center" resids
   CALL permut(da_Y, i_NumObs) ! Permute the "centered" residual values
   ! Full Model OLS:
   da_C(1:i_NumVars) = MATMUL(da_Y(1:i_NumObs), da_ADataLSQ(1:i_NumObs,1:i_NumVars))
   da_B(1:i_NumVars,1:i_NumVars) = MATMUL(TRANSPOSE(da_ADataLSQ(1:i_NumObs,1:i_NumVars)),  &
                           da_ADataLSQ(1:i_NumObs,1:i_NumVars))
   CALL invert(i_NumVars, da_B(1:i_NumVars,1:i_NumVars), iEr)
   IF (iEr /= i_OK) THEN   ! in the case of a singular matrix, can't invert
      GOTO 90000   ! ... error exit ...
   END IF
   da_X(1:i_NumVars) = MATMUL(da_C(1:i_NumVars), da_B(1:i_NumVars,1:i_NumVars))
   da_YH(1:i_NumObs) = MATMUL(da_X(1:i_NumVars), &
            TRANSPOSE(da_ADataLSQ(1:i_NumObs,1:i_NumVars)))
   da_E(1:i_NumObs) = da_Y(1:i_NumObs) - da_YH(1:i_NumObs)
   d_SSR = DOT_PRODUCT(da_E(1:i_NumObs), da_E(1:i_NumObs))
   !  Reduced Model OLS:
   da_CRed(1:i_NumVRed) = MATMUL(da_Y(1:i_NumObs), da_RedA(1:i_NumObs,1:i_NumVRed))
   da_BRed(1:i_NumVRed,1:i_NumVRed) = MATMUL(TRANSPOSE(da_RedA(1:i_NumObs,1:i_NumVRed)), &
                              da_RedA(1:i_NumObs,1:i_NumVRed))
   CALL invert(i_NumVRed, da_BRed(1:i_NumVRed,1:i_NumVRed), iEr)
   IF (iEr /= i_OK) THEN   ! in the case of a singular matrix, can't invert
      GOTO 90000   ! ... error exit ...
   END IF
   da_XRed(1:i_NumVRed) = MATMUL(da_CRed(1:i_NumVRed), da_BRed(1:i_NumVRed,1:i_NumVRed))
   da_YH(1:i_NumObs) = MATMUL(da_XRed(1:i_NumVRed), &
            TRANSPOSE(da_RedA(1:i_NumObs,1:i_NumVRed)))
   da_E(1:i_NumObs) = da_Y(1:i_NumObs) - da_YH(1:i_NumObs)
   d_SSRRed = DOT_PRODUCT(da_E(1:i_NumObs), da_E(1:i_NumObs))
   ! New test stat based on comparison of d_To (from 1st Regression pair,
   ! above loop) with d_Tstar, based on current regression pair.
   d_Tstar = (d_SSRRed-d_SSR) / d_SSR
   IF (d_Tstar >= d_To) i_CntStat = i_CntStat + 1
   call rchkusr()
END DO MainPermLoop
! ------------------- END main LSQ permutation loop --------
        ! P-Values of test is simple proportion:
d_PValueReduced = DBLE(i_CntStat)/DBLE(i_NumPermut)
90000 CONTINUE
IF (ALLOCATED(da_B           )) DEALLOCATE(da_B,            STAT=ios)
IF (ALLOCATED(da_Y           )) DEALLOCATE(da_Y,            STAT=ios)
IF (ALLOCATED(da_C           )) DEALLOCATE(da_C,            STAT=ios)
IF (ALLOCATED(da_RedA        )) DEALLOCATE(da_RedA,         STAT=ios)
IF (ALLOCATED(da_RedResids1st)) DEALLOCATE(da_RedResids1st, STAT=ios)
IF (ALLOCATED(da_BRed        )) DEALLOCATE(da_BRed,         STAT=ios)
IF (ALLOCATED(da_CRed        )) DEALLOCATE(da_CRed,         STAT=ios)
IF (ALLOCATED(da_Bin50       )) DEALLOCATE(da_Bin50,        STAT=ios)
        ! Return original (unpermuted) values to calling procedure:
IF (ALLOCATED(da_X1st)) THEN
   da_X(1:i_NumVars) = da_X1st(1:i_NumVars)
   d_SSR = d_SSR1st
   DEALLOCATE(da_X1st, STAT=ios)
ELSE
   da_X = -HUGE(d_ONE) ! absurd value
   d_SSR = -HUGE(d_ONE) ! absurd value
END IF
IF (ALLOCATED(da_XRed1st)) THEN
   da_XRed(1:i_NumVRed) = da_XRed1st(1:i_NumVRed)
   d_SSRRed = d_SSRRed1st
   DEALLOCATE(da_XRed1st, STAT=ios)
ELSE
   da_XRed = -HUGE(d_ONE) ! absurd value
   d_SSRRed = -HUGE(d_ONE) ! absurd value
END IF
RETURN   ! ... normal exit ...
END SUBROUTINE lsqleastdp


!==============================================================================

SUBROUTINE rqbr(m, nn, m5, n3, n4, a, b, t, ift, x, e, s, wa, wb, nsol,  sol, &
      lsol, l_ci1)
!d USE debugmod, ONLY: MyDebug                                         ! debug!
!___Imported Parameters and Variables:
USE jonsmodule, ONLY: lf_Equals
        ! ^-- lf_Equals: Generic test for equality of two objects.
!___Imported Procedures:
!__(none)__
IMPLICIT NONE
!___Dummy Arguments:
INTEGER,          INTENT(IN)     :: m
INTEGER,          INTENT(IN OUT) :: nn
INTEGER,          INTENT(IN OUT) :: m5
INTEGER,          INTENT(IN OUT) :: n3
INTEGER,          INTENT(IN OUT) :: n4
REAL (KIND(0D0)), INTENT(IN)     :: a(:,:)  ! a(m,nn)
REAL (KIND(0D0)), INTENT(IN)     :: b(:)    ! b(m)
REAL (KIND(0D0)), INTENT(IN OUT) :: t
INTEGER,          INTENT(OUT)    :: ift
REAL (KIND(0D0)), INTENT(OUT)    :: x(:)    ! x(nn)
REAL (KIND(0D0)), INTENT(OUT)    :: e(:)    ! e(m)
INTEGER,          INTENT(OUT)    :: s(:)    ! s(m)
REAL (KIND(0D0)), INTENT(OUT)    :: wa(:,:) ! wa(m5,n4)
REAL (KIND(0D0)), INTENT(OUT)    :: wb(:)   ! wb(m)
INTEGER,          INTENT(IN OUT) :: nsol
REAL (KIND(0D0)), INTENT(OUT)    :: sol(:,:)   ! sol(n3,nsol)
INTEGER,          INTENT(OUT)    :: lsol
LOGICAL,          INTENT(IN OUT) :: l_ci1
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     SUBROUTINE rqbr
!  Code converted using TO_F90 by Alan Miller
!  Date: 2001-06-21  Time: 14:01:04
!  Mod: Use both f90 from "TO_F90" program and original Ratfor to make
!      this Fortran 95 version. - JDR  22 Jun 2001
!     Last change:  JDR  26 Jun 2001    2:34 pm

!___Procedure Parameters:
REAL (KIND(0D0)), PARAMETER :: d_ZERO = 0.0D0
REAL (KIND(0D0)), PARAMETER ::  d_ONE = 1.0D0
REAL (KIND(0D0)), PARAMETER ::  d_TWO = 2.0D0
!___Local Variables:
REAL (KIND(0D0)) :: a1, aux, b1, d, dif, pivot, smax
REAL (KIND(0D0)) :: t0, t1, tnt
REAL (KIND(0D0)) :: toler, big
REAL (KIND(0D0)) :: dMin, dMax, d_half
REAL (KIND(0D0)) :: dSum
REAL (KIND(0D0)) :: d_Temp, d_m
! RogK: REAL (KIND(0D0)) :: tnew, told, tn, cutoff
! RogK: REAL (KIND(0D0)) :: tn
INTEGER :: i, j, k, kl, kount, kr, l, m1, m2, m3, m4
! RogK: INTEGER :: kd
INTEGER :: n, n1, n2
INTEGER :: iin, iout
INTEGER :: idxcf
!d INTEGER :: ios                                                      ! debug!
LOGICAL :: l_stage, l_test, l_init, l_iend, l_up
LOGICAL :: l_ci2, l_skip
!___Intrinsic Procedures:
INTRINSIC :: ABS, DBLE, EPSILON, HUGE, SIGN, SUM

! Compute these machine constants here:
! toler, smallest detectable |x-y|/x machine precision to the 2/3
toler = (EPSILON(d_ONE))**(d_TWO/3.0D0)
! big, a very large positive number
big = HUGE(d_ONE)
! Initialization.
ift    = 0
d_m = DBLE(m)
n   = nn
m1  = m + 1
n1  = n + 1
n2  = n + 2
m2  = m + 2
m3  = m + 3
m4  = m + 4
! Check dimensions (from calling procedure).
IF (m5 /= m+5) ift = 3
IF (n3 /= n+3) ift = 4
IF (n4 /= n+4) ift = 5
IF (m <= d_ZERO  .OR.  n <= d_ZERO) ift = 6
IF (ift > d_TWO) RETURN ! dimensions inconsistent, error return
e   = d_ZERO ! e(m)
s   = 0      ! s(m)
sol = d_ZERO ! sol(n3,nsol)
wa  = d_ZERO ! wa(m5,n4)
wb  = d_ZERO ! wb(m)
x   = d_ZERO ! x(nn)
kount  = 0
kl     = 0
kr     = 0
iin    = 0
iout   = 0
idxcf  = 0
a1     = d_ZERO
aux    = d_ZERO
b1     = d_ZERO
d      = d_ZERO
dif    = d_ZERO
dSum   = d_ZERO
d_Temp = d_ZERO
pivot  = d_ZERO
smax   = d_ZERO
t0     = d_ZERO
t1     = d_ZERO
! RogK: tnew   = d_ZERO
! RogK: tn     = d_ZERO
tnt    = d_ZERO
d_half = d_ONE/d_TWO
dMax   = -HUGE(d_ZERO)
dMin   =  HUGE(d_ZERO)
l_ci2  = .FALSE.
l_iend = .TRUE.
l_skip = .FALSE.
l_up   = .TRUE.
wa(m+2,nn+1) = d_ONE  ! Start with Solution Exit Code = OK
        ! Specified value of t outside [0,1] means do all quantiles.
IF (t < d_ZERO  .OR.  t > d_ONE) THEN  ! Compute All Quantiles.
   t0 = d_ONE/d_m - toler
   t1 = d_ONE - t0
   t  = t0
   l_iend = .FALSE.
   l_ci1  = .FALSE.
END IF
loop_num_0 : DO
   DO i=1,m     ! load up the work array.
      k = 1
      DO j=1,nn
         IF (k <= nn) THEN
            IF (j == idxcf) THEN
              l_skip = .TRUE.
            ELSE
              l_skip = .FALSE.
            END IF
            IF (.NOT.l_skip) THEN
               wa(i,k) = a(i,j)
               k = k + 1
            END IF
         END IF
         call rchkusr()
      END DO
      wa(i,n4) = n + i
      wa(i,n2) = b(i)       ! Observed Y values.

      wa(i,n1) = wa(i,n2) - wa(i,n3)
      IF (wa(i,n1) < d_ZERO) THEN
         DO j=1,n4
            wa(i,j) = -wa(i,j)
         END DO
      END IF
   END DO
   DO j=1,n
      wa(m4,j) = j
      wa(m2,j) = d_ZERO
      wa(m3,j) = d_ZERO
      DO i=1,m
         aux = SIGN(d_ONE, wa(m4,j))*wa(i,j)
         wa(m2,j) = wa(m2,j) + aux*(d_ONE-SIGN(d_ONE, wa(i,n4)))
         wa(m3,j) = wa(m3,j) + aux*SIGN(d_ONE, wa(i,n4))
         call rchkusr()
      END DO
      wa(m3,j) = d_TWO*wa(m3,j)
   END DO
   dif = d_ZERO
   l_init = .FALSE.
   ! Compute the column means.
   IF (.NOT.l_ci2) THEN
      DO k=1,n
         d_Temp = SUM(a(1:m,k))

         wa(m5,k) = d_Temp/d_m
      END DO
   END IF
   lsol  = 1    ! Number of actual solutions here is 1.
   kount = 0
   loop_num_1 : DO
      ! Compute new marginal costs.
      DO j=1,n
         wa(m1,j) = wa(m2,j) + wa(m3,j)*t
      END DO
      IF (.NOT.l_init) THEN
         ! STAGE 1
         ! Determine the vector to enter the basis.
         l_stage = .TRUE.
         kr = 1
         kl = 1
         GOTO 30
      END IF
23052 CONTINUE  ! repeat #2
      ! loop_num_2 : DO
         ! STAGE 2
         l_stage = .FALSE.
23055    CONTINUE  ! repeat #3
         ! loop_num_3 : DO
            ! Determine the vector to enter the basis.
            dMax = -big
            DO j=kr,n
               d = wa(m1,j)
               IF (d < d_ZERO) THEN
                  IF (d > (-d_TWO)) THEN
                     CYCLE ! loop j=kr,n
                  ELSE
                     d = -d - d_TWO
                  END IF
               END IF
               IF (d > dMax) THEN
                  dMax = d
                  iin  = j
               END IF
            END DO
            IF (dMax <= toler) GOTO 23054
            IF (wa(m1,iin) <= d_ZERO) THEN
               DO i=1,m4
                  wa(i,iin) = -wa(i,iin)
               END DO
               wa(m1,iin) = wa(m1,iin) - d_TWO
               wa(m2,iin) = wa(m2,iin) - d_TWO
            END IF
23072       CONTINUE  ! repeat #4
            ! loop_num_4 : DO
               ! Determine the vector to leave the basis.
               k = 0
               DO i=kl,m
                  d = wa(i,iin)
                  IF (d > toler) THEN
                     k = k + 1
                     wb(k) = wa(i,n1)/d
                     s(k) = i
                     l_test = .TRUE.
                  END IF
               END DO
23079          CONTINUE  ! repeat #5
               ! loop_num_5 : DO
                  IF (k <= 0) THEN
                     l_test = .FALSE.
                  ELSE
                     dMin = big
                     DO i=1,k
                        IF (wb(i) < dMin) THEN
                           j = i
                           dMin = wb(i)
                           iout = s(i)
                        END IF
                     END DO
                     wb(j) = wb(k)
                     s(j) = s(k)
                     k = k - 1
                  END IF
! Check for linear dependence in Stage 1.
!  Note: These "break" numbers from the ratfor version, don't seem to match
!        the structure in the rqbr.f (fortran77) code from Koenker.
!        Did the ratfor preprocessor make an error, or did someone change the
!        fortran77 code?  If the fortran77 code was changed, why? Which is
!        correct?  We follow the fortran77 code version here.
!        On the other hand, my reading of the ratfor "break" may be wrong; I
!        interpret break1 to mean break out of loop1 and break5 to break out
!        of loop5.  The fortran77 code breaks out of the loops in just the
!        opposite manner: break1 seems to break out of what I see as loop5, and
!        break5 seems to break out of loop1.  Perhaps the break<number> means
!        to break out of <number> of levels of loops. - JDR  10 Jul 2001
!   It is the number of levels to break from. - JDR Nov 2003
                  IF (.NOT.l_test  .AND.  l_stage) GOTO 23081 ! "break1"
                  IF (.NOT.l_test) GOTO 23047  ! "break5"
                  pivot = wa(iout,iin)
                  IF (wa(m1,iin)-pivot-pivot <= toler) GOTO 10
                  DO j=kr,n3
                     d          = wa(iout,j)
                     wa(m1,j)   = wa(m1,j) - d - d
                     wa(m2,j)   = wa(m2,j) - d - d
                     wa(iout,j) = -d
                     call rchkusr()
                  END DO
                  wa(iout,n4) = -wa(iout,n4)
               ! END DO loop_num_5
23080          GOTO 23079 ! #5
23081          CONTINUE
               DO i=1,m4         ! for each row  to m+4 ...
                  d         = wa(i,kr)   ! ... swap columns
                  wa(i,kr)  = wa(i,iin)
                  wa(i,iin) = d
               END DO
               kr = kr + 1
               GOTO 20
               ! Pivot on wa(iout,iin)
10             CONTINUE
               DO j=kr,n3
                  IF (j /= iin) THEN
                     wa(iout,j) = wa(iout,j)/pivot
                  END IF
               END DO
               DO i=1,m3
                  IF (i /= iout) THEN
                     d = wa(i,iin)
                     DO j=kr,n3
                        IF (j /= iin) THEN
                           wa(i,j) = wa(i,j) - d*wa(iout,j)
                        END IF
                        call rchkusr()
                     END DO
                  END IF
               END DO
               DO i=1,m3
                  IF (i /= iout) THEN
                     wa(i,iin) = -wa(i,iin)/pivot
                  END IF
               END DO
               wa(iout,iin) = d_ONE/pivot
               d           = wa(iout,n4)
               wa(iout,n4) = wa(m4,iin)
               wa(m4,iin)  = d
               kount = kount + 1
               IF (.NOT.l_stage) GOTO 23074 ! EXIT loop_num_4
               ! Interchange rows in Stage 1.
               kl = kl + 1
               DO j=kr,n4
                  d           = wa(iout,j)
                  wa(iout,j)  = wa(kount,j)
                  wa(kount,j) = d
               END DO
20             CONTINUE
               IF (kount+kr == n1) GOTO 23057
30             CONTINUE
               dMax = -d_ONE
               DO j=kr,n
                  IF (ABS(wa(m4,j)) <= n) THEN
                     d = ABS(wa(m1,j))
                     IF (d > dMax) THEN
                        dMax = d
                        iin = j
                     END IF
                  END IF
                  call rchkusr()
               END DO
               IF (wa(m1,iin) < d_ZERO) THEN
                  DO i=1,m4
                     wa(i,iin) = -wa(i,iin)
                  END DO
               END IF
            ! END DO loop_num_4
23073       GOTO 23072 ! #4
23074       CONTINUE
         ! END DO loop_num_3
23056    GOTO 23055 ! #3
23057    CONTINUE
      ! END DO loop_num_2
23053 GOTO 23052 ! #2
23054 CONTINUE
      IF (kr == 1) THEN
         DO j=1,n
            d = ABS(wa(m1,j))
            IF (d <= toler  .OR.  d_TWO-d <= toler) THEN
               ift = 1
               wa(m2,nn+1) = d_ZERO
               EXIT
            END IF
            call rchkusr()
         END DO
      END IF
      kount = 0
      dSum = d_ZERO
      IF (.NOT.l_ci2) THEN
         DO i=1,kl-1
            k = wa(i,n4)*SIGN(d_ONE, wa(i,n4))
            x(k) = wa(i,n1)*SIGN(d_ONE, wa(i,n4))
            call rchkusr()
         END DO
      END IF
      DO i=1,n

         IF (.NOT.l_ci2) THEN
            dSum = dSum + x(i)*wa(m5,i) ! beta * X-bar?
            sol(i+3,lsol) = x(i)        ! Model Betas(?)
            ! RogK: ih(i,lsol) = kd
         END IF
      END DO

      IF (.NOT.l_ci2) THEN

         sol(1,lsol) = smax    ! Quantile?
         sol(2,lsol) = dSum    ! Predicted Y at X-bar?

         dSum = d_ZERO
         DO j=kl,m
            d = wa(j,n1)*SIGN(d_ONE, wa(j,n4))
            dSum = dSum + d*(smax+d_half*(SIGN(d_ONE, d)-d_ONE))
            call rchkusr()
         END DO
         sol(3,lsol) = dSum    ! solution to objective function?

      END IF
      IF (l_ci2) THEN

         a1 = d_ZERO

         IF (l_up) THEN
            smax = big
         ELSE
            smax = -big
         END IF
         DO i=1,kl-1
            k = wa(i,n4)*SIGN(d_ONE, wa(i,n4))
            sol(k,1) = wa(i,n2)*SIGN(d_ONE, wa(i,n4))
         ! RogK: sol(k,2) = wa(i,n3)*SIGN(d_ONE,wa(i,n4))/tnew
         call rchkusr()
         END DO
         DO i=kl,m
            a1 = d_ZERO
            b1 = d_ZERO
            k = wa(i,n4)*SIGN(d_ONE, wa(i,n4)) - n
            l = 1
            DO j=1,n
               IF (j == idxcf) l = l + 1
               a1 = a1 + a(k,l)*sol(j,1)
               b1 = b1 + a(k,l)*sol(j,2)
               l = l + 1
            END DO
            tnt = (b(k)-a1)/(a(k,idxcf)-b1)

         END DO

         DO i=1,m
            ! RogK: wa(i,n3) = wa(i,n3)/told*tnew
            wa(i,n1) = wa(i,n2) - wa(i,n3)
         END DO
         DO j=kr,n3
            d          = wa(iout,j)
            wa(m1,j)   = wa(m1,j) - d - d
            wa(m2,j)   = wa(m2,j) - d - d
            wa(iout,j) = -d
         END DO
         wa(iout,n4) = -wa(iout,n4)
         l_init = .TRUE.

      END IF ! (l_ci2) is true
      IF ((l_iend)  .AND.  (.NOT.l_ci2)) GOTO 40
      IF (.NOT.l_ci2) THEN
         l_init = .TRUE.
         lsol = lsol + 1      ! Increment for next actual solution number.
         IF (lsol > nsol) THEN
            ift = 7  ! Number of solutions exceeds what was expected.
            RETURN
         END IF
         s(1:m) = d_ZERO
         x(1:n) = d_ZERO
         ! Compute next theta.
         smax = d_TWO
         DO j=1,n
            b1 = wa(m3,j)
            a1 = (-d_TWO-wa(m2,j))/b1
            b1 = -wa(m2,j)/b1
            IF (a1 >= t) THEN
               IF (a1 < smax) THEN
                  smax = a1
                  dif = (b1-a1)/d_TWO
               END IF
            END IF
            IF (b1 > t) THEN
               IF (b1 < smax) THEN
                  smax = b1
                  dif = (b1-a1)/d_TWO
               END IF
            END IF
            call rchkusr()
         END DO
         tnt = smax + toler*(d_ONE+ABS(dif))
         IF (tnt >= t1+toler) l_iend = .TRUE.
         t = tnt
         IF (l_iend) t = t1
      END IF
      call rchkusr()
   END DO loop_num_1
23047 CONTINUE

   wa(m2,nn+1) = d_TWO ! premature end?
   ift = 2
   GOTO 50
40 CONTINUE
   IF (lsol > 2) THEN    ! If the actual number of solutions > 2:
      sol(1,1) = d_ZERO     ! First Quantile.
      ! Hey, We want this. Not sure why Roger zeroes it out.
      ! This was probably a mistake.
      ! RogK: ? sol(2,1) = d_ZERO  ! First Predicted Y at X-bar.
      sol(3,1) = d_ZERO     ! First solution to the objective function.
      sol(1,lsol) = d_ONE   ! Last Quantile?
      ! Hey, We want this. Not sure why Roger zeroes it out.
      ! This was probably a mistake.
      ! RogK: ? sol(2,lsol) = d_ZERO  ! Last Predicted Y at X-bar?
      sol(3,lsol) = d_ZERO  ! Last Solution to the objective function?
      ! RogK: dsol(1:m,1)      = d_ONE
      ! RogK: dsol(1:m,lsol)   = d_ZERO
      ! RogK: dsol(1:m,lsol+1) = d_ZERO
   END IF
   l = kl - 1
   DO i=1,l
     IF (wa(i,n1) < d_ZERO) THEN
        DO j=kr,n4
           wa(i,j) = -wa(i,j)
        END DO
     END IF
   END DO
50 CONTINUE
   dSum = d_ZERO
   IF (.NOT.l_ci2) THEN
      DO i=kl,m
         k = wa(i,n4)*SIGN(d_ONE, wa(i,n4))
         d = wa(i,n1)*SIGN(d_ONE, wa(i,n4))
         dSum = dSum + d*SIGN(d_ONE, d)*(d_half+SIGN(d_ONE, d)*(t-d_half))
         k = k - n
         e(k) = d
      END DO
      wa(m2,n2) = kount     ! Number of Iterations?
      wa(m1,n2) = n1 - kr   ! Rank of Design Matrix?
      wa(m1,n1) = dSum      ! Objective function?
   END IF
   !   if (wa(m2,nn+1) == d_two) exit ! loop #0
   ! In case of Premature end (exit code == 2)
   IF (lf_Equals(wa(m2,nn+1), d_TWO)) THEN
      ift = 2
             
      EXIT ! loop #0
   END IF
   IF (.NOT.l_ci1) EXIT ! loop #0
   IF (l_ci2) CYCLE ! loop #0
   l_ci2 = .TRUE.
   n = nn - 1
   n1 = n + 1
   n2 = n + 2
   n3 = n + 3
   n4 = n + 4
60 CONTINUE
   idxcf = idxcf + 1
   IF (idxcf > nn) EXIT
70 CONTINUE
call rchkusr()
END DO loop_num_0

RETURN
END SUBROUTINE rqbr

!==============================================================================

SUBROUTINE chngxon(X, N, N2C, dOld, dNew)
!___Imported Parameters and Variables:
USE mt19937, ONLY: genrand_real3
        ! ^-- Mersenne Twister Pseudo Random Number Generator
!___Imported Procedures:
!__(none)__
IMPLICIT NONE
!___Dummy Arguments:
REAL (KIND(0D0)), INTENT(INOUT) :: X(:)
INTEGER,          INTENT(IN)    :: N
INTEGER,          INTENT(IN)    :: N2C
REAL (KIND(0D0)), INTENT(IN)    :: dOld
REAL (KIND(0D0)), INTENT(IN)    :: dNew
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     SUBROUTINE permut
! DESCRIPTION:
!     Change N2C values of array X(N) from
!           1 to 0 if l_1to0 is TRUE
!           0 to 1 if l_1to0 is FALSE
! LANGUAGE:
!     Fortran 90/95
! AUTHOR:
!     Jon D. Richards, Operations Research Analyst    jon_richards@usgs.gov
!     Fort Collins Science Center                  http://www.fort.usgs.gov
!     U.S. Geological Survey  -  Biological Resources Division
! MODIFICATION HISTORY:
!   CODE HISTORY:
!     CREATED - JDR 28 Jul 2003
!     Mod - Add option to use Mersenne Twister Pseudo Random Number Generator
!            - JDR Apr 2005
!   COMMENT HISTORY:
!     CREATED - JDR 28 Jul 2003
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!___Local Variables:
REAL (KIND(0.0E0)) :: rNum
INTEGER :: i2C, iCnt, k
!___Intrinsic Procedures:
INTRINSIC :: COUNT, INT, RANDOM_NUMBER
!___Executable Statements:
IF (dOld == dNew) RETURN ! nothing to change.
IF (N2C < 1) RETURN ! Yes we have no bananas.
iCnt = COUNT(x==dOld)
IF (N2C > iCnt) THEN
   i2C = iCnt
ELSE
   i2C = N2C
END IF
iCnt = 0
DO
   rNum = genrand_real3( )
   k = INT(N*rNum) + 1
   IF (k > N) k = N
   IF (X(k) == dOld) THEN
      X(k) = dNew
      iCnt = iCnt + 1
      IF (iCnt == i2C) EXIT
   END IF
END DO
END SUBROUTINE chngxon

!==============================================================================

FUNCTION littlemedq1(i_NumObs, da_X1, d_QuantVal) RESULT(FnResult)
!___Imported Parameters and Variables:
USE blcmnmod, ONLY: d_HALF, d_ONE, d_TWO, d_ZERO
        ! ^-- Some global Blossom variables and parameters.
!___Imported Procedures:
USE jonsmodule, ONLY: hsort, lf_Equals
        ! ^-- hsort: Sort a real (KIND(0D0)) array.
        ! ^-- lf_Equals: Generic test for equality of two objects.
IMPLICIT NONE
!___Dummy Arguments:
INTEGER,          INTENT(IN) :: i_NumObs
REAL (KIND(0D0)), INTENT(IN) :: da_X1(:)
REAL (KIND(0D0)), INTENT(IN) :: d_QuantVal
!___Function Result:
REAL (KIND(0D0)) :: FnResult
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     SUBROUTINER littlemedq1
! DESCRIPTION:
!     Selections of quantiles are obtained for a univariate set of data and
!     also distances of the same set from the median.
!     NOTE:  The univariate data set size must be at least 2.
!     U.S. Geological Survey          -          Biological Resources Division
!        Input Array Dimensions:
!           da_X1        (i_NumObs)
! LANGUAGE:
!     Fortran 90/95
! AUTHOR:
!     Dr. Paul W. Mielke, Jr.
!     Modifications:
!        Jon D. Richards, Operations Research Analyst  jon_richards @ usgs.gov
!        Fort Collins Science Center                  http://www.fort.usgs.gov
!        U.S. Geological Survey  -  Biological Resources Division
! INVOKED BY:
!
! INVOKES:
!     hsort     <File: "jonsmodule.f90: hsort">     module
!     lf_Equals <File: "jonsmodule.f90: lf_Equals"> module

!___Local Variables:
REAL (KIND(0D0)) :: da_Tmp(i_NumObs)
INTEGER :: k, l
!___Intrinsic Procedures:
INTRINSIC :: DBLE, HUGE, INT, MAX, MIN, NINT
!___Executable Statements:
IF (d_QuantVal > d_ONE  .OR.  d_QuantVal < d_ZERO) THEN ! bad quantile value
   FnResult = HUGE(d_ONE)    ! set this absurdly wrong
   RETURN                    ! return with absurd answer, hope caller sees it.
END IF
da_Tmp(1:i_NumObs) = da_X1(1:i_NumObs) ! Don't mess with array X1, use temp array to sort.
CALL hsort(da_Tmp, i_NumObs) ! Sort array into ascending order.
IF (lf_Equals(d_QuantVal, d_ZERO)) THEN      ! Zero-th Quantile:
   FnResult = da_Tmp(1)
ELSE IF (lf_Equals(d_QuantVal, d_ONE)) THEN  ! 1.0-th Quantile:
   FnResult = da_Tmp(i_NumObs)
ELSE IF (lf_Equals(d_QuantVal, d_HALF)) THEN ! Median:
   k = INT(d_HALF*DBLE(i_NumObs)+0.51D0)     ! Yes, 0.51D0
   FnResult = da_Tmp(k)
   l = i_NumObs / 2
   IF (k == l) FnResult = (da_Tmp(k)+da_Tmp(k+1)) / d_TWO
ELSE                                         ! Some other Quantile:
   IF (d_QuantVal > d_HALF) THEN
      k = MIN(i_NumObs, NINT(d_QuantVal*DBLE(i_NumObs)+d_HALF))
   ELSE
      k = MAX(1, NINT(d_QuantVal*DBLE(i_NumObs)+d_HALF))
   END IF
   FnResult = da_Tmp(k)
END IF
RETURN   ! ... normal exit ...
END FUNCTION littlemedq1

!==============================================================================

END MODULE regrsmod

MODULE doladmod
IMPLICIT NONE
!  Modified this file to be a module - JDR 7/15/2002
CONTAINS

!i_NumCases, i_NV, d_Theta, i_NumPermut, i_RandNumSeed, &
!         i_NumToDrop, ia_PosToDrop, d_Toler, l_DoLadSave, i_Test, &
!         l_DoublePermutation, &
!         da_Data, &
!         d_To, d_Tn_RS, i_CntSumAVR, d_PValue, d_PVTn, &
!         da_Betas, da_BetasRed, &
!         d_SumAbsValRes, d_SumAbsValResRed, &
!         d_WtSumAbsDevsFulMod, d_WtSumAbsDevsRedMod, &
!         i_Iter, i_ExitCode, iEr

SUBROUTINE runlad(da_Data,i_NumCases, i_NumVars, d_Theta, i_NumPermut, &
	i_NumToDrop, ia_PosToDrop, i_SaveTest, i_Test, &
	l_DoublePermutation, &
	d_To, d_Tn_RS, i_CntSumAVR, d_PValue, d_PVTn, &
	da_Betas, da_RedBetas, &
	d_SumAbsValRes, d_SumAbsValResRed, &
	d_WtSumAbsDevsFulMod, d_WtSumAbsDevsRedMod, &
	i_Iter, i_ExitCode, iEr,da_STV,i_LaHy,la_HasIntercept, i_DoAllQuants,            &
      l_DoLadTest, l_DoRankScore,l_IsOLS, l_HasHypCmdLine,l_IsLaPg,i_LaPgType,ia_NumLaVars, &
      da_ResRed,da_Resids,da_Sol,i_lsol)

!___Imported Parameters and Variables:
USE ablparms, ONLY: i_DEFAULT_NUM_LAD_PERMS, i_NUM_LAD_CMDS, i_THIS_CMD,      &
      i_PREVIOUS_CMD, i_SIZE_OF_REAL8
        ! ^-- Some high-level Blossom parameters

USE blcmnmod, ONLY: cha_VarList, ch_NL, d_ONE, d_TWO, d_ZERO, i_NOT_OK,       &
      i_OK, l_CLEAR, l_Terse, ch_BLANK, l_HasTitle,    &
      ch_Output_Title, cha_CmdStack
!d USE blcmnmod, ONLY: ch_VERSION_NUM                                  ! debug!
        ! ^-- Some global Blossom variables and parameters.
USE comndmod, ONLY: i_RegressionMode, UNASSIGNED, SIMPLE_LAD, SIMPLE_QUANT,   &
      LAD_TEST, HYP_LAD_QUANT, HYP_DELETE_QM1, HYP_RANKSCORE,                 &
      HYP_RANKSCORE_DP, HYP_DOUBLE_PERM, HYP_OLS, HYP_OLS_DP, OLS,            &
      HYP_QM1_AND_DP, OLS_TEST
        ! ^-- Command type codes and command abbreviations.
USE ioformod, ONLY: ch12_OutBuff
        ! ^-- Output buffer for character output of INTEGER number.
USE jonsmodule, ONLY: ch_EM00033, ch_EM00047, ch_EM00111, ch_EM00117,         &
      ch_EM00130, ch_EM00131, ch_EM00192, ch_MAT_SINGULARITY,                 &
      ch_REGSRMOD_EM001, ch_REGSRMOD_EM002, ch_REGSRMOD_EM003,                &
      ch_REGSRMOD_EM004, ch_REGSRMOD_EM005, ch_REGSRMOD_EM006,                &
      ch_REGSRMOD_EM007, ch_REGSRMOD_EM008, ch_REGSRMOD_EM009,                &
      ch_REGSRMOD_EM010, ch_REGRSMOD_EM018,                                   &
      ch_STR0005, ch_STR0006, i_CCode,                                        &
      i_REGSRMOD_ER001, i_REGSRMOD_ER002, i_REGSRMOD_ER003, i_REGSRMOD_ER004, &
      i_REGSRMOD_ER005, i_REGSRMOD_ER006, i_REGSRMOD_ER007, i_REGSRMOD_ER008, &
      i_REGSRMOD_ER009, i_REGSRMOD_ER010, i_REGSRMOD_ER011, i_REGSRMOD_ER018
        ! ^-- Message handling, etc.
USE missvmod, ONLY: d_MISSING_VALUE
        ! ^-- values to use for missing values representation.   l_HasHypCmdLine
USE mrolamod, ONLY: cha_LAVarList, ch_DepVarNam,                      &
       i_DepVarCmdPosn, i_DO_FULL_TEST, i_DO_NOT_FULL_TEST,      &
      i_DO_RANK_SCORE_TEST, i_NO_TEST, i_MAT_SINGULARITY, &
      i_MAX_NUM_INDEP_VARS, i_MAX_POS_2DROP, i_NumRcd,           &
      i_ProgType, i_PROG_TYPE_ALLQ, i_PROG_TYPE_HYP, i_PROG_TYPE_LAD1,        &
       l_DoScreen4MsngVals, l_TrueLad,                                           &
      la_IncludeCase! , ch_LADSavFilNam
        ! ^-- Permutation, regression and other Blossom stat parameters.
!___Imported Procedures:

USE jonsmodule, ONLY: errhand, lf_Equals
        ! ^-- delemptyfile: Delete specified file if it is zero-length.
        ! ^-- emitstr: Display a string to the console output stream.
        
        ! ^-- lf_Equals: Generic test for equality of two objects.
USE regrsmod, ONLY: lad12, lsqleast, lsqleastdp, rqbr
        ! ^-- lad12: Least Absolut Deviation regression
        ! ^-- lsqleast: Ordinary Least Squares regression
        ! ^-- lsqleastdp: Ordinary Least Squares regression, double permutation
        ! ^-- rqbr: Roger Koenker's All Quantiles Regression
        ! FOR SIMULATIONS ONLY:
USE simmod, ONLY: lo_IS_SIMUL                       !SIM
        ! ^-- simulation info
USE csgamma, ONLY: CS_Prob                          !SIM
        ! ^-- FOR SIMULATIONS ONLY
IMPLICIT NONE
!___Dummy Arguments:

INTEGER,          INTENT(IN)    :: i_NumCases
INTEGER,          INTENT(IN)    :: i_NumVars
REAL (KIND(0D0)), INTENT(IN)    :: d_Theta
INTEGER,          INTENT(INOUT)    :: i_NumPermut
INTEGER,          INTENT(INOUT)    :: i_NumToDrop !eventually should be intent in only
INTEGER,          INTENT(INOUT)    :: ia_PosToDrop(:)
INTEGER,  		  INTENT(INOUT)    :: i_LaHy
INTEGER,          INTENT(INOUT)    :: i_SaveTest
LOGICAL,          INTENT(IN)    :: l_DoublePermutation
INTEGER,          INTENT(INOUT)    :: i_Test
REAL (KIND(0D0)), INTENT(INOUT) :: da_Data(:,:)
REAL (KIND(0D0)), INTENT(OUT)   :: d_To
REAL (KIND(0D0)), INTENT(OUT)   :: d_Tn_RS
INTEGER,          INTENT(OUT)   :: i_CntSumAVR
REAL (KIND(0D0)), INTENT(OUT)   :: d_PValue
REAL (KIND(0D0)), INTENT(OUT)   :: d_PVTn
REAL (KIND(0D0)), INTENT(OUT)   :: da_Betas(:)
REAL (KIND(0D0)), INTENT(OUT)   :: da_RedBetas(:)
REAL (KIND(0D0)), INTENT(OUT)   :: d_SumAbsValRes
REAL (KIND(0D0)), INTENT(OUT)   :: d_SumAbsValResRed
REAL (KIND(0D0)), INTENT(OUT)   :: d_WtSumAbsDevsFulMod
REAL (KIND(0D0)), INTENT(OUT)   :: d_WtSumAbsDevsRedMod
INTEGER,          INTENT(OUT)   :: i_Iter
INTEGER,          INTENT(OUT)   :: i_ExitCode
INTEGER,          INTENT(OUT)   :: iEr
REAL (KIND(0D0)), INTENT(OUT)   :: da_STV(:)
LOGICAL,          INTENT(INOUT)    :: la_HasIntercept(:)
INTEGER,          INTENT(INOUT)    :: i_DoAllQuants
LOGICAL,          INTENT(INOUT)    :: l_DoLadTest
LOGICAL,          INTENT(INOUT)    :: l_DoRankScore
LOGICAL,          INTENT(INOUT)    :: l_IsOLS
LOGICAL,          INTENT(INOUT)    :: l_HasHypCmdLine
LOGICAL,          INTENT(INOUT)    :: l_IsLaPg
INTEGER, 		  INTENT(INOUT)    :: i_LaPgType
INTEGER,  		  INTENT(INOUT)    :: ia_NumLaVars(:)
REAL (KIND(0D0)), INTENT(OUT)      :: da_ResRed(:)     ! (1:i_NumObs)
REAL (KIND(0D0)), INTENT(OUT)      :: da_Resids(:)     ! (1:i_NumObs)
REAL (KIND(0D0)), INTENT(INOUT)      :: da_Sol(:,:)
INTEGER,          INTENT(INOUT)      :: i_lsol

!___Procedure Parameters:
REAL (KIND(0D0)),  PARAMETER ::   d_CONSTANT = d_ONE ! Forces regression const
CHARACTER (LEN=*), PARAMETER ::  ch_PROB_MSG = "Problem reading TEMP-DAT.TMP"
CHARACTER (LEN=*), PARAMETER :: ch_PROC_NAME = "RUNLAD"
INTEGER, PARAMETER :: i_LEN_MY_EMSG = 254 ! needs to be this big for output of var list in dostatus.
CHARACTER (LEN=i_LEN_MY_EMSG) :: ch_MyMsg
!p LOGICAL, PARAMETER :: l_DO_TIMER = .TRUE.
!___Local Variables:

        ! dual solution from L1 for Rank Score test (LAD):
REAL (KIND(0D0)), ALLOCATABLE :: da_DualSoln(:)   ! (1:i_NumObs)
        ! array of least square residuals (LAD,LSQ):
REAL (KIND(0D0)), ALLOCATABLE :: da_E(:)          ! (1:i_NumObs)
        ! array of residuals of reduced model (LAD):

        ! rank scores (LAD):
REAL (KIND(0D0)), ALLOCATABLE :: da_RankScore(:)  ! (1:i_NumObs)
        ! least square regression coefficients (LSQ):
REAL (KIND(0D0)), ALLOCATABLE :: da_X(:)          ! (1:i_NumVars)
        ! dependent variable observed values (LAD,LSQ):
REAL (KIND(0D0)), ALLOCATABLE :: da_Y(:)          ! (1:i_NumObs)
        ! dependent variable estimated values (LSQ):
REAL (KIND(0D0)), ALLOCATABLE :: da_YH(:)         ! (1:i_NumObs)
        ! dependent variable observed for reduced model
        ! All Quantile Regression arrays:
        !       m = number of cases
        !      nn = number of model parameters
        !      m5 = m + 5
        !      n3 = n + 3
        !      n4 = n + 4
INTEGER,          ALLOCATABLE :: ia_s(:)     ! s(1:m)
REAL (KIND(0D0)), ALLOCATABLE :: da_b(:)     ! b(1:m)
REAL (KIND(0D0)), ALLOCATABLE :: da_wb(:)    ! wb(1:m)
REAL (KIND(0D0)), ALLOCATABLE :: da_wa(:,:)  ! wa(1:m5,1:n4)
REAL (KIND(0D0)), ALLOCATABLE :: da_Values(:)
REAL (KIND(0D0)) :: d_t

INTEGER :: i_ToDot
        ! ^-- FOR SIMULATIONS ONLY
REAL (KIND(0D0)) :: d_MeanSqError ! Avg Abs Val of Residual (LSQ)
REAL (KIND(0D0)) :: d_To_LQ ! test stat (non-permuted) observed (LAD)
REAL (KIND(0D0)) :: d_PValueFull ! P-Value of the Full Model (LAD,LSQ)
REAL (KIND(0D0)) :: d_PValueReduced ! P-Value of the Reduced Model (LAD,LSQ)
REAL (KIND(0D0)) :: d_PValue_RS ! P-value of rank score test. (LAD)
REAL (KIND(0D0)) :: d_R         ! Pearson correlation coefficient (LSQ)
REAL (KIND(0D0)) :: d_RR        ! square of Pearson correlation coefficient (LSQ)
REAL (KIND(0D0)) :: d_SSR       ! sum squares residuals (LSQ)
REAL (KIND(0D0)) :: d_SSRRed    ! sum squares residuals for reduced model (LSQ)
REAL (KIND(0D0)) :: d_ObsStat ! observed statistic
INTEGER :: i_RandNumSeed
INTEGER :: i_CntStat   ! counter for computing test statistic differences
        ! i_CntSumAVR: count of times the sum of the absolute values of the
        ! residuals of the permuted data set regression is less than or equal
        ! to the sum of the absolute values of the residuals of the original
        ! (non-permuted) data set. (LAD,LSQ)
INTEGER :: i_Exc
INTEGER :: i_DatRow, i_DatCol ! size of row, col of data array (allocation).
INTEGER :: i, j, k, l, m,i_TempRecLen
INTEGER :: ios,i_temp_unit
INTEGER :: i_n3, i_n4, i_m5
INTEGER :: i_Count
INTEGER :: i_nsol
INTEGER :: i_MaxToDrop, i_Ndx, i_NV, i_NumVRed
LOGICAL :: l_ComputePearsonR
LOGICAL :: l_HasMissingThisCase
LOGICAL :: l_DoAllQuants
LOGICAL :: l_SaveTest
LOGICAL :: l_ci1,l_HasHoldFile
REAL (KIND(0D0)) :: d_To_RS     ! Observerd Rank Score test statistic (LAD)
CHARACTER (LEN=8) :: ch_Temp
CHARACTER (LEN=4) :: blcmd
INTEGER :: ia_LADVarNdx(i_NUM_LAD_CMDS,i_MAX_NUM_INDEP_VARS)


REAL (KIND(0D0)) :: d_Sum,d_Toler

!___Intrinsic Procedures:
INTRINSIC :: ALLOCATED, EPSILON, HUGE, LEN_TRIM, TRIM

iEr = i_OK ! no problem yet

ALLOCATE(da_Values(1:i_NumVars), STAT=ios)
IF (ios /= i_OK) THEN
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |_____________
!  |                                                |
!  | 'Memory allocation error: da_Values in RUNLAD' |
!  |________________________________________________|
!
   CALL errhand(Ch_MyMsg,i_Code=ios,         & !* <File: "jonsmodule.f90: errhand">
                ch_Msg=ch_EM00033,  &
                 ch_S1="da_Values", &
                 ch_S2=ch_PROC_NAME)
   iEr = i_NOT_OK
   GOTO 7000
END IF
! If zero cases, can't do, e.g, missing values for vars selected.
IF (i_NumCases == 0) THEN
!   _________________________________________
!  |                                         |
!  | 'No cases with non-missing values left' |
!  |_________________________________________|
!
   CALL errhand(Ch_MyMsg,ch_Msg=ch_EM00130)
   iEr = i_NOT_OK
   GOTO 7000
END IF

IF (i_DoAllQuants==1) THEN
	l_DoAllQuants=.TRUE.
	ELSE
 	l_DoAllQuants=.FALSE.
END IF

IF (i_SaveTest==1) THEN
	l_SaveTest=.TRUE.
	ELSE
 	l_SaveTest=.FALSE.
END IF


i_MaxToDrop = ia_NumLaVars(1) + 1 ! (Plus the constant position)

i_TempRecLen = i_NumVars * i_SIZE_OF_REAL8
i_RandNumSeed=-1


IF (la_HasIntercept(i_LaHy)) THEN
   i_NV = ia_NumLaVars(i_LaHy) + 1   ! Account for the CONSTANT regression var.
ELSE
   i_NV = ia_NumLaVars(i_LaHy)
END IF
IF (.NOT.la_HasIntercept(i_THIS_CMD)) THEN
   IF (.NOT.la_HasIntercept(i_PREVIOUS_CMD)) THEN
      i_NV = ia_NumLaVars(i_THIS_CMD)
   END IF
END IF
! NOW: Even for test, use the full model data.
IF (la_HasIntercept(i_THIS_CMD)) i_NV = ia_NumLaVars(i_THIS_CMD) + 1
i_NumVRed = i_NV - i_NumToDrop  ! number of independent vars in reduced model

i_Test = i_NO_TEST
IF (l_DoLadTest  .AND.  (i_LaHy == i_PROG_TYPE_LAD1)) THEN
   i_Test = i_DO_FULL_TEST        ! We will test the Full model.
ELSE
   i_Test = i_DO_NOT_FULL_TEST    ! We won't test the Full model.
END IF
IF (l_DoRankScore) THEN ! Do Rank Score test of Hypothesis (reduced model) test
   i_Test = i_DO_RANK_SCORE_TEST
END IF
IF (l_IsOLS) THEN   ! Least Squares Regression:
   i_DatRow = i_NumCases
   i_DatCol = i_NV + 1
   ALLOCATE(da_YH(1:i_NumCases),               &
            da_b(1:1),                        &
           da_wb(1:1),                        &
           da_wa(1:1,1:1),                    &
            ia_s(1:1),                        &
         STAT=ios)
ELSE IF (l_DoAllQuants) THEN
   i_DatRow = i_NumCases
   i_DatCol = i_NV
   i_nsol = 4*i_NumCases    ! A hopefull guess, may not be big enough!
   i_m5 = i_NumCases + 5
   i_n3 = i_NV + 3
   i_n4 = i_NV + 4

   ALLOCATE(                                  &
            da_b(1:i_NumCases),               & ! observed y vector........
           da_wb(1:i_NumCases),               &
           da_wa(1:i_m5,        1:i_n4),      &
            ia_s(1:i_NumCases),               &
           da_YH(1:1),                        &
         STAT=ios)
ELSE               ! LAD/Quant Regression:
   i_DatRow = i_NumCases + 2
   i_DatCol = i_NV + 2
   ALLOCATE(                                  &
           da_YH(1:1),                        &
            da_b(1:1),                        &
           da_wb(1:1),                        &
           da_wa(1:1,1:1),                    &
            ia_s(1:1),                        &
         STAT=ios)
END IF
!d CALL MyDebugNumber('  after DATA allocations, ios',ios)             ! debug!
IF (ios /= i_OK) THEN
   iEr = i_NOT_OK
   GOTO 7000
END IF

IF (l_DoRankScore) THEN ! Rank Score test:
   ALLOCATE(                          &
          da_DualSoln(1:i_NumCases),  &
         da_RankScore(1:i_NumCases),  &
         STAT=ios)
ELSE
   ALLOCATE(                 &
          da_DualSoln(1:1),  &
         da_RankScore(1:1),  &
         STAT=ios)
END IF
!d CALL MyDebugNumber('  after rankscore allocations, ios',ios)        ! debug!
IF (ios /= i_OK) THEN
   CALL errhand(ch_MyMsg,i_Code=ios,        &
                ch_Msg=ch_EM00033, &
                 ch_S1="da_DualSoln,da_RankScore", &
                 ch_S2=ch_PROC_NAME)
   iEr = i_NOT_OK
   GOTO 7000
END IF
IF (l_IsOLS  .OR.  l_DoRankScore  .OR. l_DoAllQuants) THEN
   ALLOCATE(                 &
         da_E(1:i_NumCases), &
         da_X(1:i_NV),       &
         STAT=ios)
ELSE
   ALLOCATE(        &
         da_E(1:1), &
         da_X(1:1), &
         STAT=ios)
END IF
!d CALL MyDebugNumber('  after E,X allocations, ios',ios)              ! debug!
IF (ios /= i_OK) THEN
   CALL errhand(ch_MyMsg,i_Code=ios,        &
                ch_Msg=ch_EM00033, &
                 ch_S1="da_E,da_X", &
                 ch_S2=ch_PROC_NAME)
   iEr = i_NOT_OK
   GOTO 7000
END IF

IF (((i_LaPgType == i_PROG_TYPE_LAD1  .OR.   &
      i_LaPgType == i_PROG_TYPE_HYP)  .OR.   &
      l_DoRankScore)  .AND.  .NOT.l_DoAllQuants) THEN
   ALLOCATE(da_Y(1:i_NumCases), STAT=ios)
ELSE
   ALLOCATE(da_Y(1:1), STAT=ios)
END IF

IF (ios /= i_OK) THEN
   CALL errhand(ch_MyMsg,i_Code=ios,        &
                ch_Msg=ch_EM00033, &
                 ch_S1="da_Y", &
                 ch_S2=ch_PROC_NAME)
   iEr = i_NOT_OK
   GOTO 7000
END IF

IF (ios /= i_OK) THEN
   CALL errhand(ch_MyMsg,i_Code=ios,        &
                ch_Msg=ch_EM00033, &
                 ch_S1="da_BetasRed", &
                 ch_S2=ch_PROC_NAME)
   iEr = i_NOT_OK
   GOTO 7000
END IF
! Get the data.
! Don't use cases with missing values.
i_NumRcd = 0
l_HasHoldFile = .FALSE.
!     If (la_HasIntercept(iUseNdx)) then

! the index i_Ndx to 1 (LAD.
i_Ndx = 1



d_Toler = (EPSILON(d_ONE))**(d_TWO/3.0D0) ! About: 3.666852862501036E-11
l_ComputePearsonR = .FALSE. ! We won't compute by default.

IF (l_IsOLS) THEN ! Ordinary Least SQuares.

   IF (.NOT.l_HasHypCmdLine) THEN
      IF (.NOT.(l_DoLadTest  .AND.  &
          (i_LaHy == i_PROG_TYPE_LAD1))  .AND.  &
          .NOT.(l_DoRankScore)) THEN
         i_NumPermut = 0
      END IF
   ELSE
      IF (i_NumPermut == 0) THEN
         i_NumPermut   = i_DEFAULT_NUM_LAD_PERMS ! # permutations for testing.
      END IF
   END IF

   IF (l_DoublePermutation) THEN
      i_RegressionMode = HYP_OLS_DP

      CALL lsqleastdp(i_NumCases, i_NV, i_NumPermut, i_RandNumSeed,           &
            i_NumToDrop, ia_PosToDrop, da_Data, da_X, da_RedBetas, d_SSRRed,  &
            d_SSR, d_MeanSqError, da_YH, da_E, i_CntStat, d_PValueReduced,    &
            d_To, iEr,ch_MyMsg)  !* <File: "regrsmod.f90: lsqleastdp">
      l_ComputePearsonR = .FALSE. ! We didn't.
      d_R = -HUGE(d_ONE) ! absurd value
      d_RR = -HUGE(d_ONE) ! absurd value
      d_PValueFull = -HUGE(d_ONE) ! absurd value
   ELSE
      IF (i_NumToDrop == 0) THEN
         IF (l_DoLadTest) THEN
            i_RegressionMode = OLS_TEST
         ELSE
            i_RegressionMode = OLS
         END IF
      ELSE
         i_RegressionMode = HYP_OLS
      END IF


      CALL lsqleast(i_NumCases, i_NV, i_NumPermut, i_RandNumSeed,             &
            i_NumToDrop, i_Test, ia_PosToDrop, da_Data, da_Betas, da_RedBetas,    &
            d_R, d_RR, d_SumAbsValResRed, d_SumAbsValRes, d_MeanSqError, da_YH, da_E,           &
            i_CntStat, d_PValueFull, d_PValueReduced, d_To,                   &
            l_ComputePearsonR, l_SaveTest, iEr,ch_MyMsg) !* <File: "regrsmod.f90: lsqleast">


   END IF
   ! We will want to make this call later, so set up the arguments depending
   ! on the regression mode.

      d_PVTn = d_MISSING_VALUE
      d_Tn_RS = d_MISSING_VALUE
      SELECT CASE (i_RegressionMode)
         CASE (OLS)
            d_ObsStat = d_MISSING_VALUE
            d_PValueReduced = d_MISSING_VALUE
         CASE (OLS_TEST)
            d_ObsStat = d_MISSING_VALUE
            d_PValue = d_PValueFull
         CASE (HYP_OLS)
            d_ObsStat = d_To
            d_PValue = d_PValueReduced
         CASE (HYP_OLS_DP)
            d_ObsStat = d_To
            d_PValue = d_PValueReduced
      END SELECT

ELSE IF ((i_LaPgType == i_PROG_TYPE_LAD1  .OR.       &
         i_LaPgType == i_PROG_TYPE_HYP)  .AND.       &
         .NOT.l_DoAllQuants) THEN  ! LAD/Quantile; full/reduced


   CALL lad12(i_NumCases, i_NV, d_Theta, i_NumPermut, i_RandNumSeed, &
         i_NumToDrop, ia_PosToDrop, d_Toler, l_SaveTest, i_Test, &
         l_DoublePermutation, &
         da_Data, &
         d_To, d_Tn_RS, i_CntSumAVR, d_PValue, d_PVTn, &
         da_Betas, da_RedBetas, &
         d_SumAbsValRes, d_SumAbsValResRed, &
         d_WtSumAbsDevsFulMod, d_WtSumAbsDevsRedMod, &
         i_Iter, i_ExitCode, iEr,ch_MyMsg,da_STV,i_LaHy,da_ResRed,da_Resids)
        !* <File: "regrsmod.f90: lad12">

   IF (i_NumToDrop > 0) THEN
      d_PValueReduced = d_PValue
      d_To_LQ = d_To
   ELSE
      d_PValueFull = d_PValue
   END IF
   ! assign the proper variables here (?)
   SELECT CASE (i_RegressionMode)
      CASE (      SIMPLE_LAD)
         d_PValueFull = d_PValue
      CASE (    SIMPLE_QUANT)
         d_PValueFull = d_PValue
      CASE (        LAD_TEST)
         d_PValueReduced = d_PValue
      CASE (   HYP_LAD_QUANT)
         d_PValueReduced = d_PValue
         IF (l_Terse) d_ObsStat = d_To
      CASE (  HYP_DELETE_QM1)
         d_PValueReduced = d_PValue
         IF (l_Terse) d_ObsStat = d_To
      CASE (   HYP_RANKSCORE)
         d_PValueReduced = d_PValue
         d_To_RS = d_To
         d_PValue_RS = d_PValue
         IF (l_Terse) d_ObsStat = d_To
      CASE (HYP_RANKSCORE_DP)
         d_PValueReduced = d_PValue
         d_To_RS = d_To
         d_PValue_RS = d_PValue
         IF (l_Terse) d_ObsStat = d_To
      CASE ( HYP_DOUBLE_PERM)
         d_PValueReduced = d_PValue
         IF (l_Terse) d_ObsStat = d_To
      CASE (  HYP_QM1_AND_DP)
         d_PValueReduced = d_PValue
         IF (l_Terse) d_ObsStat = d_To
   END SELECT
ELSE IF (l_DoAllQuants) THEN
   l_ci1 = .FALSE.
   d_t = -1.0D0
   da_b = da_Data(:,i_NV+1)

   CALL rqbr(i_NumCases, i_NV, i_m5, i_n3, i_n4, da_Data(1:i_NumCases,1:i_NV),&
         da_b, d_T, iEr, da_X, da_E, ia_s, da_wa, da_wb, i_nsol, da_Sol,      &
         i_lsol, l_ci1) !* <File: "regrsmod.f90: rqbr">

END IF
IF (iEr /= i_OK) THEN         ! BEGIN Error Handling
   SELECT CASE (iEr)  ! Error handling
      CASE (i_REGSRMOD_ER001)
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |______________
!  |                                                 |
!  | 'Memory allocation error: da_DepVarVals in LAD' |
!  |_________________________________________________|
!
         CALL errhand(ch_MyMsg,i_Code=i_CCode, & !* <File: "jonsmodule.f90: errhand">
                      ch_Msg=ch_REGSRMOD_EM001)
      CASE (i_REGSRMOD_ER002)
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |____________
!  |                                               |
!  | 'Memory allocation error: da_AData_RS in LAD' |
!  |_______________________________________________|
!
         CALL errhand(ch_MyMsg,i_Code=i_CCode, &
                      ch_Msg=ch_REGSRMOD_EM002)
      CASE (i_REGSRMOD_ER003)
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |__________
!  |                                             |
!  | 'Memory allocation error: ia_Work in LEAST' |
!  |_____________________________________________|
!
         CALL errhand(ch_MyMsg,i_Code=i_CCode, &
                      ch_Msg=ch_REGSRMOD_EM003)
      CASE (i_REGSRMOD_ER004)
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |______________
!  |                                                 |
!  | 'Memory allocation error: da_DataSave in LEAST' |
!  |_________________________________________________|
!
         CALL errhand(ch_MyMsg,i_Code=i_CCode, &
                      ch_Msg=ch_REGSRMOD_EM004)
      CASE (i_REGSRMOD_ER005)
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |____________________________________
!  |                                                                       |
!  | 'Memory allocation error: da_DataRed,da_DataSaveRed,da_DepVarValsRed, |
!  |   da_ResSaveRed in LEAST'                                             |
!  |_______________________________________________________________________|
!
         CALL errhand(ch_MyMsg,i_Code=i_CCode, &
                      ch_Msg=ch_REGSRMOD_EM005)
      CASE (i_REGSRMOD_ER006)
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |________________________________
!  |                                                                   |
!  | 'Memory alocation error: da_B,da_E,da_YH,da_X,da_C in RSOLSLEAST' |
!  |___________________________________________________________________|
!
         CALL errhand(ch_MyMsg,i_Code=i_CCode, &
                      ch_Msg=ch_REGSRMOD_EM006)
      CASE (i_REGSRMOD_ER007)
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |_____________________________________
!  |                                                                        |
!  | 'Memory allocation error da_RedA,da_ResRed1st,da_DepVarValsRed,da_CRed |
!  |                       in RSOLSLEAST'                                   |
!  |________________________________________________________________________|
!
         CALL errhand(ch_MyMsg,i_Code=i_CCode, &
                      ch_Msg=ch_REGSRMOD_EM007)
      CASE (i_REGSRMOD_ER008)
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |____________________
!  |                                                       |
!  | 'Memory allocation error: da_B,da_Y,da_C in LSQLEAST' |
!  |_______________________________________________________|
!
         CALL errhand(ch_MyMsg,i_Code=i_CCode, &
                      ch_Msg=ch_REGSRMOD_EM008)
      CASE (i_REGSRMOD_ER009)
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |______________________________________
!  |                                                                         |
!  | 'Memory allocation error: da_RedA,da_ResRed1st,da_DepVarValsRed,da_CRed |
!  |                       in LSQLEAST'                                      |
!  |_________________________________________________________________________|
!
         CALL errhand(ch_MyMsg,i_Code=i_CCode, &
                      ch_Msg=ch_REGSRMOD_EM009)
      CASE (i_MAT_SINGULARITY)
!   _____________________________________________________
!  |                                                     |
!  | 'Either all the elements of some row were zero or'  |
!  | 'a pivot became relatively zero.'                   |
!  | 'The variance/covariance matrix of variable values' |
!  | 'is probably singular and cannot be inverted.'      |
!  | 'Hotelling commensuration cannot be done.'          |
!  |_____________________________________________________|
!
         CALL errhand(ch_MyMsg,i_Code=i_CCode, &
                      ch_Msg=ch_MAT_SINGULARITY)
      CASE (i_REGSRMOD_ER010)
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |______________
!  |                                                 |
!  | 'Memory allocation error: da_Betas1st in LEAST' |
!  |_________________________________________________|
!
         CALL errhand(ch_MyMsg,i_Code=i_CCode, &
                      ch_Msg=ch_REGSRMOD_EM010)
      CASE (i_REGSRMOD_ER011)
         IF (l_IsOLS) THEN  ! Least Squares subroutine lsqleast
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |_____________________________
!  |                                                                |
!  | 'Problem creating LAD SAVE file in LSQLEAST: ' ch_SaveTestFile |
!  |________________________________________________________________|
!
            ch_Temp = "LSQLEAST"
         ELSE               ! LAD subroutine least
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |__________________________
!  |                                                             |
!  | 'Problem creating LAD SAVE file in LEAST: ' ch_SaveTestFile |
!  |_____________________________________________________________|
!
            ch_Temp = "LEAST"
         END IF

      CASE (i_REGSRMOD_ER018)
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |
!  |                                  |
!  | 'Problem occurred in RQBR!'      |
!  |__________________________________|
!
         CALL errhand(ch_MyMsg,i_Code=i_CCode, &
                      ch_Msg=ch_REGRSMOD_EM018)
      CASE DEFAULT
   END SELECT

END IF                        ! END Error Handling

7000 CONTINUE
80000 continue


IF (ALLOCATED(da_Values)   ) DEALLOCATE(da_Values,    STAT=ios)
IF (ALLOCATED(da_YH)       ) DEALLOCATE(da_YH,        STAT=ios)
IF (ALLOCATED(da_DualSoln) ) DEALLOCATE(da_DualSoln,  STAT=ios)
IF (ALLOCATED(da_RankScore)) DEALLOCATE(da_RankScore, STAT=ios)
IF (ALLOCATED(da_E)        ) DEALLOCATE(da_E,         STAT=ios)
IF (ALLOCATED(da_X)        ) DEALLOCATE(da_X,         STAT=ios)
                                                         ! debug!


IF (ALLOCATED(da_Y)        ) DEALLOCATE(da_Y,         STAT=ios)
IF (ALLOCATED(da_b)        ) DEALLOCATE(da_b,         STAT=ios)
IF (ALLOCATED(da_wb)       ) DEALLOCATE(da_wb,        STAT=ios)
IF (ALLOCATED(da_wa)       ) DEALLOCATE(da_wa,        STAT=ios)
IF (ALLOCATED(ia_s)        ) DEALLOCATE(ia_s,         STAT=ios)

 IF (iEr /= i_OK) THEN
   call rexit(ch_MyMsg)
END IF

l_SAVETEST = .FALSE. ! done with this until next time it is invoked
RETURN   ! ... normal exit ...
END SUBROUTINE runlad



END MODULE doladmod


MODULE covermod
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     MODULE covermod
! DESCRIPTION
!     Perform coverage tests GSECT1, EGSECT1, and KSGF.
! LANGUAGE:
!     Fortran 90/95
! AUTHOR:
!     Jon D. Richards, Operations Research Analyst       jon_richards@usgs.gov
!     Fort Collins Science Center                     http://www.fort.usgs.gov
!     U.S. Geological Survey
! MODIFICATION HISTORY:
!     Module created - JDR  20 Oct 1999
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
IMPLICIT NONE
SAVE
CONTAINS
!     GSECT1           <File: "covermod.f90: gsect1">
!        GCOMB         <File: "covermod.f90: gcomb">
!     EGSECT1          <File: "covermod.f90: egsect1">
!        EGSECT1PERM   <File: "covermod.f90: egsect1perm">
!        EGSECT1DVAL   <File: "covermod.f90: egsect1dval">
!     KSGF             <File: "covermod.f90: ksgf">

!=============================================================================!


!=============================================================================!


SUBROUTINE runksgf(da_ObserV, &
          d_TObs,    &
          d_TEst,    &
          d_VarT,    &
          l_OkZ,     &
          d_Z,       &
          l_OkSkwT,  &
          d_SkwT,    &
          d_PValue,  &
          iEr,       &
          d_ArcIntv, &
          l_DoArc,   &
          i_NumCases)
!d USE debugmod, ONLY: MyDebug                                         ! debug!
!___Imported Parameters and Variables:
USE blcmnmod, ONLY: cha_CmdStack, cha_VarList, i_NOT_OK,          &
      i_OK, l_CLEAR, l_Terse
        ! ^-- Some global Blossom variables and parameters.
USE ioformod, ONLY: ch12_OutBuff
        ! ^-- Output buffer variables for character output of numbers.
USE jonsmodule, ONLY: ch_covermod_EM005, ch_EM00033, ch_EM00050, ch_EM00052,  &
      ch_EM00053, ch_EM00106, ch_EM00111, i_CCode, i_covermod_ER005
        ! ^-- Message handling, etc.
USE missvmod, ONLY: d_MISSING_VALUE
        ! ^-- values to use for missing values representation.
USE mrolamod, ONLY: ch_CovVarNam, ch_GrpVarNam, ch_NOT_GVAR_NAME,l_IsKSGF

USE jonsmodule, ONLY: errhand, hsort, lf_Equals
        ! ^-- hsort: sort a real (KIND(0D0)) array
        ! ^-- lf_Equals: Generic test for equality of two objects.
IMPLICIT NONE
!___Dummy Arguments:
REAL (KIND(0D0)), INTENT(INOUT) :: da_ObserV(:)
REAL (KIND(0D0)), INTENT(OUT)   :: d_Tobs
REAL (KIND(0D0)), INTENT(OUT)   :: d_Test
REAL (KIND(0D0)), INTENT(OUT)   :: d_VarT
LOGICAL,          INTENT(OUT)   :: l_OkZ
REAL (KIND(0D0)), INTENT(OUT)   :: d_Z
LOGICAL,          INTENT(OUT)   :: l_OkSkwT
REAL (KIND(0D0)), INTENT(OUT)   :: d_SkwT
REAL (KIND(0D0)), INTENT(OUT)   :: d_PValue
INTEGER,          INTENT(OUT)   :: iEr
REAL (KIND(0D0)), INTENT(IN) :: d_ArcIntv
LOGICAL,          INTENT(IN)   :: l_DoArc
INTEGER,          INTENT(IN)   :: i_NumCases
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     SUBROUTINE runksgf
! DESCRIPTION:
!     Run a KSGF (Kendall-Sherman Goodness-of-Fit) test
!     Sort the data.
!     Next get cumulative values for cum. freq. distribution.
!     If ARC, get interval from highest to lowest value across arcintv
!        boundary, this is last value in cum values.
!     Check the data and limits for KSGF then call appropriate stat program.
!     Display results and send them to an output file.
! LANGUAGE:
!     Fortran 90/95
! AUTHOR:
!     Jon D. Richards, Operations Research Analyst       jon_richards@usgs.gov
!     Fort Collins Science Center                     http://www.fort.usgs.gov
!     U.S. Geological Survey          -          Biological Resources Division
! INVOKED BY:
!     codoset       <File: "docover.f90: codoset">        module
! INVOKES:

!     dispksgf      <File: "docover.f90: dispksgf">       module
!     errhand       <File: "jonsmodule.f90: errhand">     module
!     hsort         <File: "jonsmodule.f90: hsort">       module
!     ksgf          <File: "covermod.f90: ksgf">          module
!     lf_Equals     <File: "jonsmodule.f90: lf_Equals">   module

!___Procedure Parameters:
CHARACTER (LEN=*), PARAMETER :: ch_PROC_NAME = "RUNSKSGF"
!___Local Variables:
REAL (KIND(0D0)), ALLOCATABLE :: da_Values(:)
INTEGER :: i, j
INTEGER :: ios
INTEGER :: i_CovVarNum
INTEGER :: i_Exc
INTEGER :: i_NumIntv
LOGICAL :: l_HasMissingValueThisCase  ! not used here (yet)
CHARACTER (LEN=4) :: blcmd
INTEGER, PARAMETER :: i_LEN_MY_EMSG = 254 ! needs to be this big for output of var list in dostatus.
CHARACTER (LEN=i_LEN_MY_EMSG) :: ch_MyMsg
!___Intrinsic Procedures:
INTRINSIC :: ALLOCATED
!___Executable Statements:

iEr = i_OK ! no problem yet
! Get the position of the cover variable names in
! the USE file var list.

IF (l_DoArc) THEN
   i_NumIntv = i_NumCases - 1
ELSE
   i_NumIntv = i_NumCases
END IF
! Find the cumulative distribution, which we send to KSGF.
! Compute this for ARC, as this is a handy thing to have
!  in Blossom that most statistics packages won't have.
! For non-ARC, the user is supposed to have this computed,
!  so the data should be that cummulative distribution.
ALLOCATE(da_Values(1:i_NumCases), STAT=ios)
IF (ios /= i_OK) THEN
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |______________
!  |                                                 |
!  | 'Memory allocation error da_Values in GETCOVAL' |
!  |_________________________________________________|
!
   CALL errhand(ch_MyMsg,i_Code=ios,         &
                ch_Msg=ch_EM00033,  &
                 ch_S1="da_Values", &
                 ch_S2=ch_PROC_NAME)
   iEr = i_NOT_OK

   RETURN   ! ... error exit ...
END IF


IF (l_DoArc) THEN
   da_Values(1) = 1.0D0 + (da_ObserV(1)-da_ObserV(i_NumCases)) / d_ArcIntv
ELSE
   da_Values(1) = da_ObserV(1)
END IF

DO i=2,i_NumIntv
   IF (l_DoArc) THEN
      da_Values(i) = da_Values(i-1) + (da_ObserV(i) - da_ObserV(i-1)) /       &
            d_ArcIntv
   ELSE
      da_Values(i) = da_ObserV(i)
   END IF
END DO

l_IsKSGF=.TRUE.


! Kendall-Sherman Goodness-of-Fit Test
CALL ksgf(i_NumIntv, & !* <File: "covermod.f90: ksgf">
          da_Values, &
          d_TObs,    &
          d_TEst,    &
          d_VarT,    &
          l_OkZ,     &
          d_Z,       &
          l_OkSkwT,  &
          d_SkwT,    &
          d_PValue,  &
          iEr)
IF (iEr /= i_OK) THEN
   SELECT CASE (iEr)  ! Error handling
      CASE (i_covermod_ER005)
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |_______
!  |                                          |
!  | 'Memory allocation errror: da_C in KSGF' |
!  |__________________________________________|
!
         CALL errhand(ch_MyMsg,i_Code=i_CCode, &
                      ch_Msg=ch_covermod_EM005)
      CASE DEFAULT
   END SELECT
   iEr = i_NOT_OK
   RETURN   ! ... error exit ...
END IF
DEALLOCATE(da_Values, STAT=ios)
! Display results and send to output file.

 IF (iEr /= i_OK) THEN
   call rexit(ch_MyMsg)
END IF

RETURN   ! ... normal exit ...
END SUBROUTINE runksgf

SUBROUTINE gsect1(d_SKT, d_To, i_NumSimuls,d_V_DistExp,i_NumGrps,  &
                  ia_GrpSizes,da_ObsData, &
                  d_ET, d_VT,l_OkSDVT,   &
                  d_SDVT,l_OkZ,d_Z,        &
                  l_OkSKT,l_OkPVal,   &
                  d_PVal, d_PZ, iEr,        &
                  l_SaveTest,  da_STV ,ch_MyMsg)


USE blcmnmod, ONLY: d_ONE, d_ZERO, i_OK
USE jonsmodule, ONLY: i_CCode, i_covermod_ER001, i_covermod_ER002
USE mrolamod, ONLY: i_MIN_NUM_GPS_COV
USE jonsmodule, ONLY: lf_Equals
USE pv_modul, ONLY: covpvalue
      
IMPLICIT NONE
!___Dummy Aruguments:
REAL (KIND(0D0)), INTENT(OUT)   :: d_SKT
REAL (KIND(0D0)), INTENT(OUT)   :: d_To
INTEGER,          INTENT(IN)    :: i_NumSimuls
REAL (KIND(0D0)), INTENT(IN)    :: d_V_DistExp
INTEGER,          INTENT(IN)    :: i_NumGrps
INTEGER,          INTENT(INOUT)    :: ia_GrpSizes(:)
REAL (KIND(0D0)), INTENT(INOUT) :: da_ObsData(:)
REAL (KIND(0D0)), INTENT(OUT)   :: d_ET
REAL (KIND(0D0)), INTENT(OUT)   :: d_VT
LOGICAL,          INTENT(OUT)   :: l_OkSDVT
REAL (KIND(0D0)), INTENT(OUT)   :: d_SDVT
LOGICAL,          INTENT(OUT)   :: l_OkZ
REAL (KIND(0D0)), INTENT(OUT)   :: d_Z
LOGICAL,          INTENT(OUT)   :: l_OkSKT
LOGICAL,          INTENT(OUT)   :: l_OkPVal
REAL (KIND(0D0)), INTENT(OUT)   :: d_PVal
REAL (KIND(0D0)), INTENT(OUT)   :: d_PZ
INTEGER,          INTENT(OUT)   :: iEr
LOGICAL, 		  INTENT(INOUT) :: l_SaveTest
REAL (KIND(0D0)), INTENT(OUT) :: da_STV(:)   ! save test values
INTEGER, PARAMETER :: i_LEN_MY_EMSG = 254
CHARACTER (LEN=i_LEN_MY_EMSG), INTENT(OUT) :: ch_MyMsg !might not need this because the routine has no error messages

!___Procedure Parameters:
REAL (KIND(0D0)), PARAMETER :: d_EPSLN = 1.0D-12
!___Local Variables:
REAL (KIND(0D0)), ALLOCATABLE :: da_C(:,:)
REAL (KIND(0D0)), ALLOCATABLE :: da_X(:,:)
REAL (KIND(0D0)), ALLOCATABLE :: da_R(:)

REAL (KIND(0D0)) :: A1, A2, A3, A4, B1, B2, d_ED2, d_NumObs, d_NumObsP1
REAL (KIND(0D0)) :: d_D, d_T, W
REAL (KIND(0D0)) :: d_Expression
INTEGER :: i, j, k, l, ii, IS, MZ
INTEGER :: ios
INTEGER :: i_MaxGrpSize, i_NumObs
INTEGER :: N1
LOGICAL :: l_Undefined_Flag
!___Intrinsic Procedures:
INTRINSIC :: ALLOCATED, ABS, DBLE, HUGE, MAXVAL, SQRT, SUM
!___Executable Statements:
iEr = i_OK ! no problem yet
i_NumObs = 0

d_ET   = d_ZERO
d_PVal = d_ZERO
d_SDVT = d_ZERO
d_To   = d_ZERO
d_VT   = d_ZERO
d_Z    = d_ZERO

i_MaxGrpSize = MAXVAL(ia_GrpSizes(1:i_NumGrps)) ! What is largest group size?
i_NumObs     = SUM(ia_GrpSizes(1:i_NumGrps))    ! How many obs, total?
d_NumObs     = DBLE(i_NumObs)
d_NumObsP1   = d_NumObs + d_ONE
ALLOCATE(                                 &
      da_R(1:i_NumObs),                   &
      da_X(1:i_MaxGrpSize,1:i_NumGrps),   &
      da_C(1:i_MaxGrpSize+1,1:i_NumGrps), &
      STAT=ios)
IF (ios /= i_OK) then
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |___________________
!  |                                                      |
!  | "Memory allocation errror: da_R,da_X,da_C in GSECT1" |
!  |______________________________________________________|
!
   iEr = i_covermod_ER002
   i_CCode = ios
   RETURN   ! ... error exit ...
END IF
da_R = d_ZERO  ! initialize array to zero
da_X = d_ZERO  ! initialize array to zero
da_C = d_ZERO  ! initialize array to zero
A1 = d_ZERO
A2 = d_ZERO
A3 = d_NumObs - 0.1D0

d_SKT=d_ZERO
B1 = -1.0D30
B2 = 1.0D30
DO WHILE(A2 <= A3)
 call rchkusr()
   !  13 IF (A2 > A3) GOTO 17
   DO i=1,i_NumObs
      IF (da_ObsData(i) > B1) THEN
         IF (da_ObsData(i) < B2) THEN
            B2 = da_ObsData(i) + 1.0D-8
         END IF
      END IF
   END DO
   DO i=1,i_NumObs
      W = ABS(da_ObsData(i)-B2)
      IF (W < 1.0D-7) A1 = A1 + 1
   END DO
   ! should this be:
   !  A4 = A2 + (A1+1.0D0)/2.0D0
   A4 = A2 + (A1+1)/2
   DO i=1,i_NumObs
   call rchkusr()
      W = ABS(da_ObsData(i)-B2)
      IF (W < 1.0D-7) da_R(i) = A4
   END DO
   A2 = A2+A1
   A1 = d_ZERO
   B1 = B2
   B2 = 1.0D30
   !     GOTO 13
END DO
!  17 CONTINUE
k = 0
DO i=1,i_NumGrps
   DO j=1,ia_GrpSizes(i)
   call rchkusr()
      k = k + 1
      da_X(j,i) = da_R(k)
   END DO
END DO
DO i=1,i_NumGrps  ! This is a sort.
call rchkusr()
   j = ia_GrpSizes(i)
   k = j/2 + 1
21 k = k - 1
   W = da_X(k,i)
   GOTO 23
22 W = da_X(j,i)
   da_X(j,i) = da_X(1,i)
   j = j - 1
23 ii = k
24 l = 2*ii
   IF (l-j) 25,26,27
25 IF (da_X(l+1,i) >= da_X(l,i)) l = l + 1
26 IF (W >= da_X(l,i)) GOTO 27
   da_X(ii,i) = da_X(l,i)
   ii = l
   GOTO 24
27 da_X(ii,i) = W
   IF (k > 1) GOTO 21
   IF (j >= 2) GOTO 22
END DO
DO i=1,i_NumGrps
   DO j=1,ia_GrpSizes(i)+1
   call rchkusr()
      IF (j == 1) THEN
         da_C(j,i) = da_X(j,i)/d_NumObsP1
      ELSE IF (j == ia_GrpSizes(i)+1) THEN
         da_C(j,i) = d_ONE - da_X(j-1,i)/d_NumObsP1
      ELSE
         da_C(j,i) = (da_X(j,i)-da_X(j-1,i))/d_NumObsP1
      END IF
   END DO
END DO
d_ET = d_ZERO
DO i=1,i_NumGrps
   d_D = d_ZERO
   DO j=1,i_NumObs-ia_GrpSizes(i)+1
   call rchkusr()
      d_D = d_D +                                                             &
            ABS(DBLE(j)/d_NumObsP1-d_ONE/(ia_GrpSizes(i)+1))**d_V_DistExp*    &
            gcomb(i_NumObs-j,ia_GrpSizes(i)-1) !* <File: "covermod.f90: gcomb">
   END DO
   d_ET = d_ET + d_D*(ia_GrpSizes(i)+1)/gcomb(i_NumObs,ia_GrpSizes(i))
                                        !* <File: "covermod.f90: gcomb">
END DO
d_To = d_ZERO
DO i=1,i_NumGrps
   DO j=1,ia_GrpSizes(i)+1
   call rchkusr()
      d_To = d_To + ABS(da_C(j,i)-d_ONE/(ia_GrpSizes(i)+1))**d_V_DistExp

   END DO
END DO
d_To  = d_To*(d_ONE-d_EPSLN)
IF (l_SAVETEST) THEN ! We want to save original and all resampled test statistics
    da_STV(1) = d_To
END IF
MZ    = 0
d_VT  = d_ZERO
d_SKT = d_ZERO
d_ED2 = d_ZERO
DO IS=1,i_NumSimuls-1
   call rchkusr()
    CALL gsapwor(i_NumObs, &
                 i_NumObs, &
                 da_R)
   N1 = 0
   DO i=1,i_NumGrps
      DO j=1,ia_GrpSizes(i)
      call rchkusr()
         N1 = N1 + 1
         da_X(j,i) = da_R(N1)
      END DO
   END DO
   
   DO i=1,i_NumGrps   ! This is a sort.
   call rchkusr()
      j = ia_GrpSizes(i)
      k = j/2 + 1
111   k = k - 1
      W = da_X(k,i)
      GOTO 113
112   W = da_X(j,i)
      da_X(j,i) = da_X(1,i)
      j  = j - 1
113   ii = k
114   l  = 2*ii
      IF (l-j) 115,116,117
115   IF (da_X(l+1,i) >= da_X(l,i)) l = l + 1
116   IF (W >= da_X(l,i)) GOTO 117
      da_X(ii,i) = da_X(l,i)
      ii = l
      GOTO 114
117   da_X(ii,i) = W
      IF (k > 1)  GOTO 111
      IF (j >= 2) GOTO 112
   END DO
   DO i=1,i_NumGrps
      DO j=1,ia_GrpSizes(i)+1
      call rchkusr()
         IF (j == 1) THEN
            da_C(j,i) = da_X(j,i)/d_NumObsP1
         ELSE IF (j == ia_GrpSizes(i)+1) THEN
            da_C(j,i) = d_ONE - da_X(j-1,i)/d_NumObsP1
         ELSE
            da_C(j,i) = da_X(j,i)/d_NumObsP1 - da_X(j-1,i)/d_NumObsP1
         END IF
      END DO
   END DO
   d_T = d_ZERO
   DO i=1,i_NumGrps
      DO j=1,ia_GrpSizes(i)+1
      call rchkusr()
         d_T = d_T + ABS(da_C(j,i)-d_ONE/DBLE(ia_GrpSizes(i)+1))**d_V_DistExp
      END DO
   END DO


   d_VT  = d_VT  + (d_T-d_ET)**2
   d_SKT = d_SKT + (d_T-d_ET)**3
   d_ED2 = d_ED2 + (d_T-d_ET)**4

   IF (d_T > d_To) MZ = MZ + 1
   IF (l_SAVETEST) THEN ! ! Save value of test stat for this permuted sample
      da_STV(IS+1) = d_T
   END IF
END DO

d_Expression = (d_ED2*i_NumSimuls-d_VT*d_VT)/(i_NumSimuls-1)
d_Expression=d_Expression/i_NumSimuls
d_Expression=d_Expression/i_NumSimuls

IF (d_Expression < d_ZERO) THEN
   l_OkSDVT = .FALSE.
   d_SDVT = HUGE(d_ZERO)

ELSE
   l_OkSDVT = .TRUE.
   d_SDVT = SQRT(d_Expression)
END IF

d_VT   = d_VT/DBLE(i_NumSimuls)


IF (lf_Equals(d_VT, d_ZERO)) THEN !* <File: "jonsmodule.f90: lf_Equals">
   l_Undefined_Flag = .TRUE.
ELSE

   d_SKT = (d_SKT/DBLE(i_NumSimuls)) / d_VT**1.5D0
   d_Z   = (d_To-d_ET) / SQRT(d_VT)
   l_Undefined_Flag = .FALSE.
END IF
d_PZ = DBLE(MZ)/DBLE(i_NumSimuls)
IF (l_Undefined_Flag) THEN
   l_OkPVal = .FALSE.
   l_OkSKT  = .FALSE.
   l_OkZ    = .FALSE.
   d_PVal   = HUGE(d_ZERO)
   d_SKT    = HUGE(d_ZERO)
   d_Z      = HUGE(d_ZERO)
ELSE
   l_OkPVal = .TRUE.
   l_OkSKT  = .TRUE.
   l_OkZ    = .TRUE.
   d_PVal = covpvalue(d_Z, d_SKT) !* <File: "pv_modul.f90: covpvalue">
END IF

IF (ALLOCATED(da_X)) DEALLOCATE(da_X, STAT=ios)
IF (ALLOCATED(da_R)) DEALLOCATE(da_R, STAT=ios)
IF (ALLOCATED(da_C)) DEALLOCATE(da_C, STAT=ios)
END SUBROUTINE gsect1


!=============================================================================!

SUBROUTINE gsapwor(m, N, da_X)

USE mt19937, ONLY: genrand_real3
        ! ^-- Mersenne Twister Pseudo Random Number Generator
IMPLICIT NONE
!___Dummy Arguments:
INTEGER,          INTENT(IN)    :: m
INTEGER,          INTENT(IN)    :: N
REAL (KIND(0D0)), INTENT(INOUT) :: da_X(:)
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     SUBROUTINE gsapwor
! DESCRIPTION:
!     M: SIZE OF POPULATION
!     N: SIZE OF SAMPLE DRAWN WITHOUT REPLACEMENT (1 </= N </= M)
!     da_X: REAL INPUT WHICH CONTAINS da_X(1),...,da_X(M) POPULATION VALUES
!     WITHOUT REPLACEMENT SAMPLE CONTAINED IN da_X(1),...,da_X(N)
! LANGUAGE:
!     Fortran 90/95
! AUTHOR:
!     Dr. Paul W. Mielke, Jr., Dept of Statistics, Colorado State University
!        http://www.stat.colostate.edu/~mielke/mielkesoftware.html
!     Modifications:
!        Jon D. Richards, Operations Research Analyst    jon_richards@usgs.gov
!        Fort Collins Science Center                  http://www.fort.usgs.gov
!        U.S. Geological Survey  -  Biological Resources Division
! INVOKED BY:
!     gsect1 <File: "covermod.f90: gsect1"> module

! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!___Local Variables:
REAL (KIND(0D0)) :: d_T, d_Bin
INTEGER :: i, j
!___Intrinsic Procedures:
INTRINSIC :: INT, RANDOM_NUMBER
!___Executable Statements:
call rchkusr()
DO i=1,N
      d_T = genrand_real3( )
   j = INT(i+(m-i+1)*d_T)
   d_Bin   = da_X(j)
   da_X(j) = da_X(i)
   da_X(i) = d_Bin
END DO
RETURN
END SUBROUTINE gsapwor

!=============================================================================!

FUNCTION gcomb(N, m) RESULT(d_Comb)
!___Imported Parameters and Variables:
USE blcmnmod, ONLY: d_ONE, d_TWO, dPI => d_PI
        ! ^-- Some global Blossom variables and parameters.
!___Imported Procedures:
!__(none)__
IMPLICIT NONE
!___Dummy Arguments:
INTEGER, INTENT(IN) :: N
INTEGER, INTENT(IN) :: m
REAL (KIND(0D0)) :: d_Comb
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     FUNCTION gcomb
! DESCRIPTION:
!     Computations for gsect1.
! LANGUAGE:
!     Fortran 90/95

!___Local Variables:
! REAL (KIND(0D0)) :: dPI
REAL (KIND(0D0)) :: d_R, d_Z, d_D, dF, dU1, dU2, dU3
INTEGER :: i
!___Intrinsic Procedures:
! INTRINSIC ATAN
INTRINSIC :: DBLE, EXP, LOG
!___Executable Statements:
! dPI = ATAN(d_ONE)*4.0D0
d_R = LOG(dPI*d_TWO)/d_TWO
d_Z = d_ONE + DBLE(N)
d_D = d_Z
dF = d_Z + 10.0D0
DO i=1,9
   d_D = d_D*(d_Z+DBLE(i))
END DO
dU1 = (dF*d_TWO-d_ONE)*LOG(dF)/d_TWO - dF + d_R - LOG(d_D) +               &
      d_ONE/(dF*12.0D0) - d_ONE/(dF**3*360.0D0) + d_ONE/(dF**5*1260.0D0) - &
      d_ONE/(dF**7*1680.0D0) + d_ONE/(dF**9*1188.0D0)
d_Z = d_ONE + DBLE(m)
d_D = d_Z
dF = d_Z + 10.0D0
DO i=1,9
   d_D = d_D*(d_Z+DBLE(i))
END DO
dU2 = (dF*d_TWO-d_ONE)*LOG(dF)/d_TWO - dF + d_R - LOG(d_D) +               &
      d_ONE/(dF*12.0D0) - d_ONE/(dF**3*360.0D0) + d_ONE/(dF**5*1260.0D0) - &
      d_ONE/(dF**7*1680.0D0) + d_ONE/(dF**9*1188.0D0)
d_Z = d_ONE + DBLE(N-m)
d_D = d_Z
dF = d_Z + 10.0D0
DO i=1,9
   d_D = d_D*(d_Z+DBLE(i))
END DO
dU3 = (dF*d_TWO-d_ONE)*LOG(dF)/d_TWO - dF + d_R - LOG(d_D) +               &
      d_ONE/(dF*12.0D0) - d_ONE/(dF**3*360.0D0) + d_ONE/(dF**5*1260.0D0) - &
      d_ONE/(dF**7*1680.0D0) + d_ONE/(dF**9*1180.0D0)
d_Comb = EXP(dU1-dU2-dU3)
RETURN
END FUNCTION gcomb

!=============================================================================!

SUBROUTINE egsect1(i_NumGrps,   &
                   ia_GrpSizes, &
                   da_ObsData,  &
                   d_T1,        &
                   d_PVal,      &
                   iEr)

USE blcmnmod, ONLY: d_ONE, d_TWO, d_ZERO, i_OK
USE jonsmodule, ONLY: i_CCode, i_covermod_ER006

IMPLICIT NONE
!___Dummy Arguments:
INTEGER,          INTENT(IN)    :: i_NumGrps
INTEGER,          INTENT(INOUT) :: ia_GrpSizes(:)
REAL (KIND(0D0)), INTENT(IN)    :: da_ObsData(:)
REAL (KIND(0D0)), INTENT(OUT)   :: d_T1
REAL (KIND(0D0)), INTENT(OUT)   :: d_PVal
INTEGER,          INTENT(OUT)   :: iEr

REAL (KIND(0D0)), ALLOCATABLE :: da_C(:,:) ! dim(MAX(M)+1,i_NumGrps+1)
REAL (KIND(0D0)), ALLOCATABLE :: da_X(:,:) ! dim(MAX(M),i_NumGrps+1)
REAL (KIND(0D0)), ALLOCATABLE :: da_R(:)   ! dim(i_NumObs=total # obs)
REAL (KIND(0D0)) :: d_A1, d_A2, d_A3, d_A4, d_B1, d_B2, d_W
INTEGER :: i, j, k, l, ii
INTEGER :: ios ! return status code
INTEGER :: i_Kount1, i_Kount2
INTEGER :: i_MaxGrpSize ! size of largest group
INTEGER :: i_NumGrpsP1  ! i_NumGrps+1
INTEGER :: i_NumObs     ! total number of observations

INTRINSIC :: ABS, ALLOCATED, DBLE, MAXVAL, SUM

iEr = i_OK ! no problem yet
d_PVal = d_ZERO
i_NumGrpsP1 = i_NumGrps + 1
ia_GrpSizes(i_NumGrpsP1) = 0
i_MaxGrpSize = MAXVAL(ia_GrpSizes(1:i_NumGrps))
i_NumObs     = SUM(ia_GrpSizes(1:i_NumGrps))
ALLOCATE(                                   &
      da_R(1:i_NumObs),                     &
      da_C(1:i_MaxGrpSize+1,1:i_NumGrpsP1), &
      da_X(1:i_MaxGrpSize,1:i_NumGrpsP1),   &
      STAT=iEr)
IF (iEr /= i_OK) THEN
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |____________________
!  |                                                       |
!  | "Memory allocation errror: da_R,da_X,da_C in EGSECT1" |
!  |_______________________________________________________|
!
   iEr = i_covermod_ER006
   i_CCode = ios
   RETURN   ! ... error exit ...
END IF
d_A1 = d_ZERO
d_A2 = d_ZERO
d_A3 = DBLE(i_NumObs) - 0.1D0
d_B1 = -1.0D30
d_B2 = 1.0D30
DO WHILE (d_A2 <= d_A3)
   DO i=1,i_NumObs
      IF (da_ObsData(i) > d_B1) THEN
         IF (da_ObsData(i) < d_B2) THEN
            d_B2 = da_ObsData(i) + 1.0D-8
         END IF
      END IF
   END DO
   DO i=1,i_NumObs
      d_W = ABS(da_ObsData(i)-d_B2)
      IF (d_W < 1.0D-7) d_A1 = d_A1 + d_ONE
   END DO
   d_A4 = d_A2 + (d_A1+d_ONE)/d_TWO
   DO i=1,i_NumObs
      d_W = ABS(da_ObsData(i)-d_B2)
      IF (d_W < 1.0D-7) da_R(i) = d_A4
   END DO
   d_A2 = d_A2 + d_A1
   d_A1 = d_ZERO
   d_B1 = d_B2
   d_B2 = 1.0D30
END DO
k = 0
DO i=1,i_NumGrps
   DO j=1,ia_GrpSizes(i)
      k = k + 1
      da_X(j,i) = da_R(k)
   END DO
END DO
   DO i=1,i_NumGrps  ! This is a sort.
      j = ia_GrpSizes(i)
      k = j/2 + 1
10    CONTINUE
      k = k - 1
      d_W = da_X(k,i)
      GOTO 30
20    CONTINUE
      d_W = da_X(j,i)
      da_X(j,i) = da_X(1,i)
      j  = j - 1
30    CONTINUE
      ii = k
40    CONTINUE
      l  = 2*ii
      IF (l-j) 50,60,70
50    CONTINUE
      IF (da_X(l+1,i) >= da_X(l,i)) l = l + 1
60    CONTINUE
      IF (d_W >= da_X(l,i)) GOTO 70
      da_X(ii,i) = da_X(l,i)
      ii = l
      GOTO 40
70    CONTINUE
      da_X(ii,i) = d_W
      IF (k > 1)  GOTO 10
      IF (j >= 2) GOTO 20
END DO
DO i=1,i_NumGrps
   DO j=1,ia_GrpSizes(i)+1
      IF (j == 1) THEN
         da_C(j,i) = da_X(j,i) / DBLE(i_NumObs+1)
      ELSE IF (j == ia_GrpSizes(i)+1) THEN
         da_C(j,i) = d_ONE - da_X(j-1,i) / DBLE(i_NumObs+1)
      ELSE
         da_C(j,i) = (da_X(j,i)-da_X(j-1,i)) / DBLE(i_NumObs+1)
      END IF
   END DO
END DO
d_T1 = d_ZERO
DO i=1,i_NumGrps
   DO j=1,ia_GrpSizes(i)+1
      d_T1 = d_T1 + ABS(da_C(j,i) - d_ONE/DBLE(ia_GrpSizes(i)+1))
   END DO
END DO
d_T1 = d_T1*(d_ONE-1.0D-12)
iEr = i_OK
CALL egsect1perm(i_NumObs,     & !* <File: "covermod.f90: egsect1perm">
                 i_NumGrps,    &
                 i_NumGrpsP1,  &
                 i_MaxGrpSize, &
                 ia_GrpSizes,  &
                 i_Kount1,     &
                 i_Kount2,     &
                 d_T1,         &
                 iEr)
IF (iEr /= i_OK) THEN
   DEALLOCATE(da_R, da_C, da_X, STAT=ios)
   RETURN   ! ... error exit ...
END IF

d_PVal = DBLE(i_Kount2)/DBLE(i_Kount1)

IF (ALLOCATED(da_R)) DEALLOCATE(da_R, STAT=ios)
IF (ALLOCATED(da_C)) DEALLOCATE(da_C, STAT=ios)
IF (ALLOCATED(da_X)) DEALLOCATE(da_X, STAT=ios)
RETURN   ! ... normal exit ...
END SUBROUTINE egsect1

!=============================================================================!

SUBROUTINE egsect1perm(i_NumObs,     &
                       i_NumGrps,    &
                       i_NumGrpsP1,  &
                       i_MaxGrpSize, &
                       ia_B,         &
                       i_Kount1,     &
                       i_Kount2,     &
                       d_T1,         &
                       iEr)
!d USE debugmod, ONLY: MyDebug                                         ! debug!
!___Imported Parameters and Variables:
USE blcmnmod, ONLY: i_OK
        ! ^-- Some global Blossom variables and parameters.
USE jonsmodule, ONLY: i_CCode, i_covermod_ER003
        ! ^-- Message handling, etc.
!___Imported Procedures:
!__(none)__
IMPLICIT NONE
!___Dummy Arguments:
INTEGER,          INTENT(IN)    :: i_NumObs ! tot numb observations
INTEGER,          INTENT(IN)    :: i_NumGrps ! number of groups
INTEGER,          INTENT(IN)    :: i_NumGrpsP1  ! numb groups + 1
INTEGER,          INTENT(IN)    :: i_MaxGrpSize ! size largest group
INTEGER,          INTENT(IN)    :: ia_B(:) ! groupsizes dim(i_NumGrps+1)
INTEGER,          INTENT(OUT)   :: i_Kount1
INTEGER,          INTENT(OUT)   :: i_Kount2
REAL (KIND(0D0)), INTENT(IN)    :: d_T1 ! observered GSECT1 stat
INTEGER,          INTENT(INOUT) :: iEr ! error flag
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     SUBROUTINE egsect1perm
! DESCRIPTION:
!     Permutation computation for
!     Exact G-sample Emperical Coverage Test.
! LANGUAGE:
!     Fortran 90/95
! AUTHOR:
!     Dr. Paul W. Mielke, Jr., Dept of Statistics, Colorado State University
!        http://www.stat.colostate.edu/~mielke/mielkesoftware.html
!     Modifications:
!        Jon D. Richards, Operations Research Analyst    jon_richards@usgs.gov
!        Fort Collins Science Center                  http://www.fort.usgs.gov
!        U.S. Geological Survey  -  Biological Resources Division
! INVOKED BY:
!     egsect1     <File: "covermod.f90: egsect1">     module
! INVOKES:
!     egsect1dval <File: "covermod.f90: egsect1dval"> module

! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!___Local Variables:
INTEGER, ALLOCATABLE :: ia_A(:,:) ! dim(i_NumGrps+1,i_NumObs)
INTEGER, ALLOCATABLE :: ia_P(:)   ! dim(i_NumObs)
INTEGER :: i, j, k, l, m, ii, jj, ll
INTEGER :: ios
INTEGER :: i_Flag, i_Limit, i_NMarks, i_Temp
!___Intrinsic Procedures:
!__(none)__
!___Executable Statements:

!d READ*                                                               ! debug!
iEr = i_OK ! no problem yet
ALLOCATE(                             &
      ia_A(1:i_NumGrpsP1,1:i_NumObs), &
      ia_P(1:i_NumObs),               &
      STAT=iEr)
IF (iEr /= i_OK) THEN
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |___________________________
!  |                                                              |
!  | "Memory allocation errror: ia_A,ia_P in EGSECT1/egsect1perm" |
!  |______________________________________________________________|
!
   iEr = i_covermod_ER003
   i_CCode = ios
   RETURN   ! ... error exit ...
END IF
k = i_NumGrps
IF (ia_B(i_NumGrps+1) /= 0) k = k + 1
l = i_NumObs
DO i=1,k-1
   DO j=1,ia_B(i)
      ia_A(i,j) = i
   END DO
   DO j=ia_B(i)+1,l
      ia_A(i,j) = i + 1
   END DO
   l = l - ia_B(i)
END DO
i_Kount1 = 0
i_Kount2 = 0
        ! 10    CONTINUE
loop10: DO
   i = k - 1
   i_NMarks = ia_B(k) + ia_B(i)
   IF (i_Kount1 == 0) GOTO 40
        !    20 CONTINUE
   loop20: DO
      DO j=1,i_NMarks-1
         IF (ia_A(i,j) == i) THEN
            IF (ia_A(i,j+1) == (i+1)) THEN
               i_Limit     = j - 2
               ia_A(i,j)   = ia_A(i,j+1)
               ia_A(i,j+1) = ia_A(i,j) - 1
               IF (i_Limit <= 0) GOTO 40
        !                   GOTO 30
               EXIT loop20
            END IF
         END IF
         call rchkusr()
      END DO
      IF (i_NMarks /= 1) THEN
         DO j=1,i_NMarks/2
            i_Temp    = ia_A(i,j)
            ia_A(i,j) = ia_A(i,i_NMarks-j+1)
            ia_A(i,i_NMarks-j+1) = i_Temp
         END DO
      END IF
      i = i - 1
      IF (i == 0) THEN
         iEr = 0
         DEALLOCATE(ia_A, ia_P, STAT=ios)
         RETURN                           ! ... normal exit ...
      END IF
      i_NMarks = i_NMarks + ia_B(i)
        !       GOTO 20
   END DO loop20
        !    30    CONTINUE
   i_Flag = 1
   DO WHILE(i_Flag == 1)
      i_Flag = 0
      DO j=1,i_Limit
         IF (ia_A(i,j) == i+1) THEN
            IF (ia_A(i,j+1) == i) THEN
               ia_A(i,j)   = ia_A(i,j+1)
               ia_A(i,j+1) = ia_A(i,j) + 1
               i_Flag = 1
            END IF
         END IF
         call rchkusr()
      END DO
   END DO
40    CONTINUE
   ia_A(k,1:i_NumObs) = ia_A(1,1:i_NumObs)
   IF (k /= 2) THEN
      DO l=2,k-1
         m = 1
         DO j=1,i_NumObs
            IF (ia_A(k,j) == l) THEN
               ia_A(k,j) = ia_A(l,m)
               m = m + 1
            END IF
            call rchkusr()
         END DO
      END DO
   END IF
   ll = 1
   DO ii=1,k
      DO jj=1,i_NumObs
         IF (ia_A(k,jj) == ii) THEN
            ia_P(ll) = jj
            ll = ll + 1
         END IF
     END DO
   END DO

   CALL egsect1dval(i_NumObs,     & !* <File: "covermod.f90: egsect1dval">
                    i_NumGrps,    &
                    i_NumGrpsP1,  &
                    i_MaxGrpSize, &
                    ia_B,         &
                    ia_P,         &
                    i_Kount2,     &
                    d_T1,         &
                    iEr)
   IF (iEr /= i_OK) RETURN   ! ... error exit ...
   i_Kount1 = i_Kount1 + 1
        ! GOTO 10
END DO loop10
END SUBROUTINE egsect1perm

!=============================================================================!

SUBROUTINE egsect1dval(i_NumObs,     &
                       i_NumGrps,    &
                       i_NumGrpsP1,  &
                       i_MaxGrpSize, &
                       ia_GrpSizes,  &
                       ia_P,         &
                       i_Kount2,     &
                       d_T1,         &
                       iEr)
!d USE debugmod, ONLY: MyDebug                                         ! debug!
!___Imported Parameters and Variables:
USE blcmnmod, ONLY: d_ONE, d_ZERO, i_OK
        ! ^-- Some global Blossom variables and parameters.
USE jonsmodule, ONLY: i_CCode, i_covermod_ER004
        ! ^-- Message handling, etc.
!___Imported Procedures:
!__(none)__
IMPLICIT NONE
!___Dummy Arguments:
        !      DOUBLE PRECISION  d_ZERO,d_ONE,d_T,d_T1,da_C(21,11),da_X(20,11)
        !      INTEGER  ia_GrpSizes(11),ia_P(50)
INTEGER,          INTENT(IN)    :: i_NumObs ! tot numb observations
INTEGER,          INTENT(IN)    :: i_NumGrps ! number of groups
INTEGER,          INTENT(IN)    :: i_NumGrpsP1 ! numb groups + 1
INTEGER,          INTENT(IN)    :: i_MaxGrpSize ! size largest grp
INTEGER,          INTENT(IN)    :: ia_GrpSizes(:) ! dim(i_NumGrps+1)
INTEGER,          INTENT(IN)    :: ia_P(:)     ! dim(i_NumObs)
INTEGER,          INTENT(INOUT) :: i_Kount2
REAL (KIND(0D0)), INTENT(IN)    :: d_T1 ! observed GSECT1 statistic
INTEGER,          INTENT(INOUT) :: iEr ! error flag
! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     SUBROUTINE egsect1dval
! DESCRIPTION:
!     Computations for egsect1perm procedure.
! LANGUAGE:
!     Fortran 90/95
! AUTHOR:
!     Dr. Paul W. Mielke, Jr., Dept of Statistics, Colorado State University
!        http://www.stat.colostate.edu/~mielke/mielkesoftware.html
!     Modifications:
!        Jon D. Richards, Operations Research Analyst    jon_richards@usgs.gov
!        Fort Collins Science Center                  http://www.fort.usgs.gov
!        U.S. Geological Survey  -  Biological Resources Division
! INVOKED BY:
!     egsect1perm <File: "covermod.f90: egsect1perm"> module

! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!___Local Variables:
REAL (KIND(0D0)), ALLOCATABLE :: da_C(:,:)
        !                              ^--dim(i_MaxGrpSize+1,i_NumGrps+1)
REAL (KIND(0D0)), ALLOCATABLE :: da_X(:,:)
        !                              ^--dim(i_MaxGrpSize,i_NumGrps+1)
REAL (KIND(0D0)) :: d_T, d_W
INTEGER :: i, j, k, l, ii
INTEGER :: ios
INTEGER :: i_MaxGrpSizeP1, N1
!___Intrinsic Procedures:
INTRINSIC :: ABS, DBLE
!___Executable Statements:

!d READ*                                                               ! debug!
iEr = i_OK ! no problem yet
i_MaxGrpSizeP1 = i_MaxGrpSize + 1
ALLOCATE(                                 &
      da_C(1:i_MaxGrpSizeP1,1:i_NumGrpsP1), &
      da_X(1:i_MaxGrpSize,1:i_NumGrpsP1),   &
      STAT=iEr)
IF (iEr /= i_OK) THEN
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |___________________________
!  |                                                              |
!  | "Memory allocation errror: da_C,da_X in EGSECT1/egsect1dval" |
!  |______________________________________________________________|
!
   iEr = i_covermod_ER004
   i_CCode = ios
   RETURN   ! ... error exit ...
END IF
N1 = 0
DO i=1,i_NumGrps
   DO j=1,ia_GrpSizes(i)
      N1 = N1 + 1
      da_X(j,i) = DBLE(ia_P(N1))
   END DO
END DO
DO i=1,i_NumGrps        ! This is a sort.
   j  = ia_GrpSizes(i)
   k  = j/2 + 1
10    CONTINUE
   k   = k-1
   d_W = da_X(k,i)
   GOTO 30
20    CONTINUE
   d_W = da_X(j,i)
   da_X(j,i) = da_X(1,i)
   j   = j - 1
30    CONTINUE
   ii = k
40    CONTINUE
   l  = 2*ii
   IF (l-j) 50,60,70
50    CONTINUE
   IF (da_X(l+1,i) >= da_X(l,i)) l = l + 1
60    CONTINUE
   IF (d_W >= da_X(l,i)) GOTO 70
   da_X(ii,i) = da_X(l,i)
   ii = l
   GOTO 40
70    CONTINUE
   da_X(ii,i) = d_W
   IF (k > 1)  GOTO 10
   IF (j >= 2) GOTO 20
END DO
DO i=1,i_NumGrps
  DO j=1,ia_GrpSizes(i)+1
    IF (j == 1) THEN
      da_C(j,i) = da_X(j,i) / DBLE(i_NumObs+1)
    ELSE IF (j == ia_GrpSizes(i)+1) THEN
      da_C(j,i) = d_ONE - da_X(j-1,i) / DBLE(i_NumObs+1)
    ELSE
      da_C(j,i) = (da_X(j,i)-da_X(j-1,i)) / DBLE(i_NumObs+1)
    END IF
  END DO
END DO
d_T = d_ZERO
DO i=1,i_NumGrps
  DO j=1,ia_GrpSizes(i)+1
    d_T = d_T + ABS(da_C(j,i)-d_ONE/DBLE(ia_GrpSizes(i)+1))
  END DO
END DO
IF (d_T >= d_T1) i_Kount2 = i_Kount2 + 1
DEALLOCATE(da_C, da_X, STAT=ios)
RETURN   ! ... normal exit ...
END SUBROUTINE egsect1dval

!=============================================================================!

SUBROUTINE ksgf(i_NumObs,    &
                da_U_ObserV, &
                d_T,         &
                d_ET1,       &
                d_VarT,      &
                l_OkZ,       &
                d_Z,         &
                l_OkSkwT,    &
                d_SkwT,      &
                d_PVal,      &
                iEr)
!d USE debugmod, ONLY: MyDebug                                         ! debug!
!___Imported Parameters and Variables:
USE blcmnmod, ONLY: d_ONE, d_TWO, d_ZERO, i_OK
        ! ^-- Some global Blossom variables and parameters.
USE jonsmodule, ONLY: i_CCode, i_covermod_ER005
        ! ^-- Message handling, etc.
!___Imported Procedures:
USE jonsmodule, ONLY: hsort, lf_Equals
        ! ^-- hsort: Sort a REAL (KIND(0D0)) array.  (low to high values).
        ! ^-- lf_Equals: Generic test for equality of two objects.
USE pv_modul, ONLY: covpvalue
        ! ^-- covpvalue: Pearson Type III approx of P-Value for cov tests.
IMPLICIT NONE
!___Dummy Arguments:
INTEGER,          INTENT(IN)    :: i_NumObs
REAL (KIND(0D0)), INTENT(INOUT) :: da_U_ObserV(:)
REAL (KIND(0D0)), INTENT(OUT)   :: d_T
REAL (KIND(0D0)), INTENT(OUT)   :: d_ET1
REAL (KIND(0D0)), INTENT(OUT)   :: d_VarT
LOGICAL,          INTENT(OUT)   :: l_OkZ
REAL (KIND(0D0)), INTENT(OUT)   :: d_Z
LOGICAL,          INTENT(OUT)   :: l_OkSkwT
REAL (KIND(0D0)), INTENT(OUT)   :: d_SkwT
REAL (KIND(0D0)), INTENT(OUT)   :: d_PVal
INTEGER,          INTENT(OUT)   :: iEr

! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!     SUBROUTINE ksgf
! DESCRIPTION:
!     Kendall-Sherman Goodness-Of-Fit Test
!     The null hypothesis specifies that continuous random variable X has
!     cumulative distribution function F(X).  If Y1 < ... < YN are the
!     order statistics of X1, ..., XN,
!        let U(I) = F(YI) for I = 1, ..., i_NumObs.
!     The P-Value here is based on the Kendall-Sherman Goodness-of-fit
!     test statistic (d_T).  The input data file is the sequential list
!     given by i_NumObs,U(1), ..., U(i_NumObs).
!          original: DIMENSION U(300),C(301)
! LANGUAGE:
!     Fortran 90, with Lahey Computer Systems LF90 intrinsics extensions
! AUTHOR:
!     Dr. Paul W. Mielke, Jr., Dept of Statistics, Colorado State University
!        http://www.stat.colostate.edu/~mielke/mielkesoftware.html
!     Modifications:
!        Jon D. Richards, Operations Research Analyst    jon_richards@usgs.gov
!        Fort Collins Science Center                  http://www.fort.usgs.gov
!        U.S. Geological Survey  -  Biological Resources Division
! INVOKED BY:
!     runksgf   <File: "docover.f90: runksgf">      module
! INVOKES:
!     covpvalue <File: "pv_modul.f90: covpvalue">   module
!     hsort     <File: "jonsmodule.f90: hsort">     module
!     lf_Equals <File: "jonsmodule.f90: lf_Equals"> module

! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . !
!___Procedure Parameters:
REAL (KIND(0D0)), PARAMETER :: d_THREE = 3.0D0
REAL (KIND(0D0)), PARAMETER ::  d_FOUR = 4.0D0
REAL (KIND(0D0)), PARAMETER ::   d_SIX = 6.0D0
REAL (KIND(0D0)), PARAMETER :: d_EIGHT = 8.0D0
!___Local Variables:
REAL (KIND(0D0)), ALLOCATABLE :: da_C(:)
REAL (KIND(0D0)) :: d_EC, d_ET2, d_ET3, d_NumObs, d_H
INTEGER :: i
INTEGER :: ios
!___Intrinsic Procedures:
INTRINSIC :: ABS, DBLE, HUGE, SQRT, SUM
!___Executable Statements:

!d READ*                                                               ! debug!
iEr = i_OK ! no problem yet
d_ET1  = d_ZERO
d_PVal = d_ZERO
d_SkwT = d_ZERO
d_T    = d_ZERO
d_VarT = d_ZERO
d_Z    = d_ZERO
ALLOCATE(da_C(1:i_NumObs+1), STAT=ios )
IF (ios /= i_OK) then
!   __________________________________
!  |                                  |
!  | IOSTAT_MSG(i_Code) error message |_______
!  |                                          |
!  | "Memory allocation errror: da_C in KSGF" |
!  |__________________________________________|
!
   iEr = i_covermod_ER005
   i_CCode = ios
   RETURN   ! ... error exit ...
END IF


d_NumObs = DBLE(i_NumObs)
        !x. CALL ORDER(i_NumObs, da_U_ObserV)
CALL hsort(da_U_ObserV, & !* <File: "jonsmodule.f90: hsort">
           i_NumObs) ! Sort array da_U_ObserV into ascending order.


da_C(1) = da_U_ObserV(1)
DO i=2,i_NumObs
   da_C(i) = da_U_ObserV(i) - da_U_ObserV(i-1)
END DO
da_C(i_NumObs+1) = d_ONE - da_U_ObserV(i_NumObs)
d_EC   = d_ONE / DBLE(i_NumObs+1)
d_T    = SUM(ABS(da_C(1:i_NumObs+1)-d_EC))
d_H    = d_NumObs + d_ONE
d_ET1  = d_TWO * (d_NumObs/d_H)**(i_NumObs+1)
d_ET2  = d_FOUR * ((d_TWO*(d_NumObs/d_H)**(i_NumObs+2) +                      &
         d_NumObs*((d_H-d_TWO)/d_H)**(i_NumObs+2))/(d_H+d_ONE))
d_ET3  = d_EIGHT * (d_SIX*((d_NumObs/d_H)**(i_NumObs+3) +                     &
         d_NumObs*((d_H-d_TWO)/d_H)**(i_NumObs+3))  +                         &
         d_NumObs * (d_NumObs-d_ONE)*((d_NumObs-d_TWO)/d_H)**(i_NumObs+3)) /  &
         (d_H+d_ONE) / (d_H+d_TWO)
d_VarT = d_ET2 - d_ET1**2
d_SkwT = (d_ET3-d_THREE*d_ET2*d_ET1+d_TWO*d_ET1**3) / d_VarT**1.5D0
d_Z    = (d_T-d_ET1) / SQRT(d_VarT)

! If the variance of d_T (variable d_VarT) is zero then the values of
! d_Z and d_SkwT are undefined and we can't get the P-Value from the
! covpvalue function. So set d_PVal to 1.0,
! and flag d_SkwT and d_Z as undefined.
IF (lf_Equals(d_VarT, d_ZERO)) THEN !* <File: "jonsmodule.f90: lf_Equals">
   l_OkSkwT = .FALSE.
   l_OkZ    = .FALSE.
   d_SkwT   = HUGE(d_ONE)
   d_Z      = HUGE(d_ONE)
   d_PVal   = d_ONE
ELSE
   l_OkSkwT = .TRUE.
   l_OkZ    = .TRUE.
   d_SkwT   = (d_ET3-d_THREE*d_ET2*d_ET1+d_TWO*d_ET1**3) / d_VarT**1.5D0
   d_Z      = (d_T-d_ET1) / SQRT(d_VarT)
   d_PVal   = covpvalue(d_Z, d_SkwT) !* <File: "pv_modul.f90: covpvalue">

END IF


RETURN   ! ... normal exit ...
END SUBROUTINE ksgf



!=============================================================================!

SUBROUTINE rungsect(d_SKT, d_Delta, i_NumSimuls,d_V_DistExp,i_NumGrps,  &
                  ia_GrpSizes,da_ObsData, &
                  d_ET, d_VT,l_OkSDVT,   &
                  d_SDVT,l_OkZ,d_Z,        &
                  l_OkSKT,l_OkPVal,   &
                  d_PValue, d_PZ, iEr,        &
                  l_SaveTest,  da_STV ,i_NumGroups,l_DoEGSECT,da_GpVals)

USE blcmnmod, ONLY:  i_NOT_OK, i_OK        
USE jonsmodule, ONLY: ch_covermod_EM002,                   &
      ch_covermod_EM003, ch_covermod_EM004, ch_covermod_EM006,    &
      i_CCode,           &
      i_covermod_ER002, i_covermod_ER003, i_covermod_ER004, i_covermod_ER006
USE mrolamod, ONLY: i_NumMVs

USE jonsmodule, ONLY: errhand
        

IMPLICIT NONE

!___Dummy Aruguments:
REAL (KIND(0D0)), INTENT(OUT)   :: d_SKT
REAL (KIND(0D0)), INTENT(OUT)   :: d_Delta
INTEGER,          INTENT(INOUT)    :: i_NumSimuls
REAL (KIND(0D0)), INTENT(IN)    :: d_V_DistExp
INTEGER,          INTENT(IN)    :: i_NumGrps
INTEGER,          INTENT(INOUT)    :: ia_GrpSizes(:)
REAL (KIND(0D0)), INTENT(INOUT) :: da_ObsData(:)
REAL (KIND(0D0)), INTENT(OUT)   :: d_ET
REAL (KIND(0D0)), INTENT(OUT)   :: d_VT
LOGICAL,          INTENT(OUT)   :: l_OkSDVT
REAL (KIND(0D0)), INTENT(OUT)   :: d_SDVT
LOGICAL,          INTENT(OUT)   :: l_OkZ
REAL (KIND(0D0)), INTENT(OUT)   :: d_Z
LOGICAL,          INTENT(OUT)   :: l_OkSKT
LOGICAL,          INTENT(OUT)   :: l_OkPVal
REAL (KIND(0D0)), INTENT(OUT)   :: d_PValue
REAL (KIND(0D0)), INTENT(OUT)   :: d_PZ
INTEGER,          INTENT(OUT)   :: iEr
LOGICAL, 		  INTENT(INOUT) :: l_SaveTest
REAL (KIND(0D0)), INTENT(OUT) :: da_STV(:)   ! save test values
INTEGER,          INTENT(IN)    :: i_NumGroups
LOGICAL,          INTENT(IN)   :: l_DoEGSECT
REAL (KIND(0D0)), INTENT(IN)   :: da_GpVals(:)

CHARACTER (LEN=*), PARAMETER :: ch_PROC_NAME = "RUNGSECT"
!___Local Variables:
!! REAL (KIND(0D0)), ALLOCATABLE :: da_ObsData(:)

INTEGER :: i
INTEGER :: ios
INTEGER :: i_ActiveCases
INTEGER :: i_Exc

CHARACTER (LEN=4) :: blcmd
INTEGER, PARAMETER :: i_LEN_MY_EMSG = 254 ! needs to be this big for output of var list in dostatus.
CHARACTER (LEN=i_LEN_MY_EMSG) :: ch_MyMsg
!___Intrinsic Procedures:
INTRINSIC :: ALLOCATED
!___Executable Statements:

iEr = i_OK ! no problem yet
call rchkusr()

i_NumMVs = 1

call rchkusr()
IF (l_DoEGSECT) THEN
   ! EXACT G-Sample Empirical Coverage Test (Univariate)
 
   CALL egsect1(i_NumGroups, & 
                ia_GrpSizes,   &
                da_ObsData,   &
                d_Delta,      &
               d_PValue,    &
                iEr)
      
   IF (iEr /= i_OK) THEN
      SELECT CASE (iEr)  ! Error handling
         CASE (i_covermod_ER006)
         
            call errhand(ch_MyMsg,i_Code=i_CCode, &
                         ch_Msg=ch_covermod_EM006)
         CASE (i_covermod_ER003)

            call errhand(ch_MyMsg,i_Code=i_CCode, &
                         ch_Msg=ch_covermod_EM003)
         CASE (i_covermod_ER004)
            call errhand(ch_MyMsg,i_Code=i_CCode, &
                         ch_Msg=ch_covermod_EM004)
         CASE DEFAULT
      END SELECT
      iEr = i_NOT_OK
      RETURN   ! ... error exit ...
   END IF
ELSE
   ! G-Sample Empirical Coverage Test (Univariate)


   CALL gsect1(d_SKT,d_Delta,i_NumSimuls,d_V_DistExp, i_NumGroups, &
               ia_GrpSizes, da_ObsData,       &
               d_ET,      d_VT,      l_OkSDVT,    &
               d_SDVT,    l_OkZ,       d_Z,         &
               l_OkSKT,   l_OkPVal,    &
               d_PValue,  d_PZ,   iEr, l_SaveTest,  &
               da_STV ,ch_MyMsg) 

   IF (iEr /= i_OK) THEN
      SELECT CASE (iEr)  ! Error handling
         CASE (i_covermod_ER002)

            call errhand(ch_MyMsg,i_Code=i_CCode, &
                         ch_Msg=ch_covermod_EM002)
         CASE DEFAULT
      END SELECT
      iEr = i_NOT_OK
      l_SAVETEST = .FALSE. ! done with this until next time it is invoked
      RETURN   ! ... error exit ...
   END IF
END IF

 IF (iEr /= i_OK) THEN
   call rexit(ch_MyMsg)
END IF

l_SAVETEST = .FALSE. ! done with this until next time it is invoked
RETURN   ! ... normal exit ...
END SUBROUTINE rungsect

END MODULE covermod

!============================================================================!
subroutine WrapEMRPP(da_Data,ia_GrpSizes, l_HasExcess,d_ExcessVal,l_DoHot,l_IsCommens, &
	  d_Trunc,d_ArcIntv,      &
      d_V, i_CForm, i_NumGrps,i_NumObs,           &
      i_NumVars, da_GpVals, i_ActiveCases, da_GroupV, d_Delta1, da_YHot,      &
      d_PValue,iEr,da_CommAvgDist,iGrpSize)
USE mrppmod !might need to list all modules called by mrpp


!USE mrppmod
implicit none

INTEGER :: i_NumVars, i_CForm, i_LZ, i_NumPerms, iEr, i_NumObs, i_NumGrps,i_ActiveCases,iGrpSize
REAL (KIND(0D0)) :: d_V,d_NumObs, d_Sup, d_NumGrps, d_Trunc,d_ArcIntv
REAL (KIND(0D0)) :: d_T, d_DBar, d_D1, d_Var, d_Gam, d_PValue, d_Delta1,d_ExcessVal
REAL (KIND(0D0)) :: da_Data(i_NumObs,(i_NumVars+1)), da_XI1(i_NumGrps),da_YHot(i_NumVars,i_NumVars)
REAL (KIND(0D0)) :: da_GroupV(i_NumObs),da_GpVals(i_NumGrps)
LOGICAL :: l_DoResample, l_HasExcess,l_DoHot,l_IsCommens
INTEGER :: ia_GrpSizes(iGrpSize)
REAL (KIND(0D0)), INTENT(OUT) :: da_CommAvgDist(1:i_NumVars)
i_ActiveCases=i_NumObs

CALL runemrpp(da_Data,ia_GrpSizes, l_HasExcess,d_ExcessVal,l_DoHot,l_IsCommens, d_Trunc,d_ArcIntv,      &
      d_V, i_CForm, i_NumGrps,           &
      i_NumVars, da_GpVals, i_ActiveCases, da_GroupV, d_Delta1, da_YHot,      &
      d_PValue,iEr,da_CommAvgDist)

END subroutine WrapEMRPP
!============================================================================!
!============================================================================!
subroutine WrapMRPP(i_NumObs, i_NumVars, d_V, d_Sup, i_CForm, l_DoHot,l_IsCommens, i_NumGrps,    &
      d_NInterv, ia_GrpSizes, i_NumPerms, l_DoResample, da_Data, da_XI1,     &
      d_T, d_DBar, d_D1, d_Var, d_Gam, d_PValue, da_YHot, iEr,    &
      d_ExcessVal,l_HasExcess,da_GpVals,ia_GrpValTag,da_GroupV,l_SaveTest,da_STV,da_CommAvgDist,iGrpSize)


USE mrppmod !might need to list all modules called by mrpp

!USE mrppmod
implicit none

INTEGER :: i_NumVars, i_CForm, i_NumPerms, iEr, i_NumObs, i_NumGrps,iGrpSize
REAL (KIND(0D0)) :: d_V,d_NumObs, d_Sup, d_NumGrps, d_NInterv
REAL (KIND(0D0)) :: d_T, d_DBar, d_D1, d_Var, d_Gam, d_PValue, d_Delta1,d_ExcessVal
REAL (KIND(0D0)) :: da_Data(i_NumObs,(i_NumVars+1)), da_XI1(i_NumGrps),da_YHot(i_NumVars,i_NumVars)
REAL (KIND(0D0)) :: da_GpVals(i_NumGrps), da_GroupV(i_NumObs)
LOGICAL :: l_DoResample, file_exists, l_HasExcess, l_DoHot, l_IsCommens
INTEGER :: ia_GrpSizes(iGrpSize), i_ActiveCases, ia_GrpValTag(i_NumObs)
LOGICAL, 		  INTENT(IN) :: l_SaveTest
REAL (KIND(0D0)), INTENT(OUT) :: da_STV(i_NumPerms),da_CommAvgDist(1:i_NumVars)

INTEGER :: i_Seed
d_NumObs=i_NumObs
d_NumGrps=i_NumGrps
i_ActiveCases=d_NumObs


i_seed=-1
call rchkusr()
CALL runmrpp(da_Data, iEr, i_NumObs, i_NumVars, d_V, d_Sup, i_CForm, l_DoHot,l_IsCommens, i_NumGrps,    &
      d_NInterv, ia_GrpSizes, i_NumPerms, l_DoResample,  da_XI1,     &
      d_T, d_DBar, d_D1, d_Var, d_Gam, d_PValue, da_YHot, l_HasExcess,       &
      d_ExcessVal, da_GpVals, i_ActiveCases, da_GroupV,ia_GrpValTag,i_Seed,l_SaveTest,da_STV,da_CommAvgDist)

END subroutine WrapMRPP
!=============================================================================
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine WrapMRBP(da_Data, d_V, i_NumVars, i_NumBlocks,          &
      i_NumGrps, i_NumPerms, i_NumObs, da_GpVals, da_BlockV, da_AlignVals,   &
      ch_BlkVarNam,l_IsAligned,l_DoEMRBP,l_DoResample, l_IsCommens, d_ObsDelta, d_ExpDelta,d_VarDelta,d_SkwDelta, &
      d_AgreeVal,d_StdStat, d_PValue,iEr,da_CommAvgDist,l_SaveTest,da_STV)


USE mrppmod !might need to list all modules called by mrpp

implicit none

INTEGER :: i_NumVars, i_CForm, i_LZ, i_NumPerms, iEr, i_NumObs, i_NumGrps, i_NumBlocks
REAL (KIND(0D0)) :: d_V,d_NumObs, d_Sup, d_NumGrps, d_NInterv, d_ExcessVal,dPRankExp
REAL (KIND(0D0)) :: d_T, d_DBar, d_D1, d_Var, d_Gam, d_PValue, d_ObsDelta,d_ExpDelta, d_VarDelta, d_SkwDelta, d_AgreeVal, d_StdStat
REAL (KIND(0D0)) :: da_Data(i_NumGrps,i_NumBlocks,i_NumVars), da_XI1(i_NumGrps),da_YHot(i_NumVars,i_NumVars)
REAL (KIND(0D0)) :: da_CommAvgDist(i_NumVars)
REAL (KIND(0D0)) :: da_GpVals(i_NumGrps), da_alignvals(i_NumBlocks,i_NumVars),da_blockV(i_NumObs)
LOGICAL :: l_DoResample, file_exists, l_HasExcess, l_IsAligned,l_DoEMRBP, l_IsCommens
INTEGER :: ia_GrpSizes(i_NumGrps), i_ActiveCases, ia_GrpValTag(i_NumObs)
INTEGER, PARAMETER :: i_LEN_MY_EMSG = 254 ! needs to be this big for output of var list in dostatus.
CHARACTER (LEN=*) :: ch_BlkVarNam
REAL (KIND(0D0)) :: da_STV(i_NumPerms)  ! save test values
LOGICAL :: l_SaveTest
INTEGER :: i_seed

i_seed=-1

CALL runmrbp(da_Data, d_V, i_NumVars, i_NumBlocks,          &
      i_NumGrps, i_NumPerms,da_GpVals, da_BlockV, da_AlignVals,              &
      ch_BlkVarNam, l_IsAligned,l_DoEMRBP,l_DoResample, l_IsCommens, d_ObsDelta, d_ExpDelta,d_VarDelta,d_SkwDelta, &
      d_AgreeVal,d_StdStat, d_PValue,iEr,da_CommAvgDist,l_SaveTest,da_STV,i_seed)

END subroutine WrapMRBP
!=============================================================================
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine WrapPTMP(i_NumPairs, d_V, iPRank, dPRankExp, da_D1, da_D2, &
            d_Expected_Delta, d_Var, d_SkwDelta, d_StdStat, d_Rho, d_ObsDelta, &
            d_PValue, l_DoResample,l_ExactPTMPDone, i_NumPerms,iEr, l_SaveTest,da_STV)


USE mrppmod !might need to list all modules called by mrpp

implicit none

INTEGER :: i_NumPairs, iPRank, i_NumPerms, iEr, i_NumObs, i_NumGrps, i_NumBlocks
REAL (KIND(0D0)) :: d_V, dPRankExp, da_D1(i_NumPairs), da_D2(i_NumPairs), d_Expected_Delta
REAL (KIND(0D0)) :: d_Var, d_SkwDelta, d_StdStat, d_Rho, d_ObsDelta, d_PValue
LOGICAL :: l_DoResample,l_ExactPTMPDone
REAL (KIND(0D0)) :: da_STV(i_NumPerms)  ! save test values
LOGICAL :: l_SaveTest
INTEGER :: i_seed
i_seed=-1

CALL runptmp(i_NumPairs, d_V, iPRank, dPRankExp, da_D1, da_D2, &
            d_Expected_Delta, d_Var, d_SkwDelta, d_StdStat, d_Rho, d_ObsDelta, &
            d_PValue, l_DoResample,l_ExactPTMPDone, i_NumPerms,iEr, l_SaveTest,da_STV,i_seed)


END subroutine WrapPTMP


!--------------------------------------------------------------------------------------

subroutine WrapMRSP(i_NumObs, i_NumVars, d_V, i_NumPerms, l_DoResample, l_DoEMRSP,l_IsCommens,da_Data,   &
      d_TestStat,d_ObsDelta,d_ExpDelta,d_VarDelta,d_SkwDelta,d_RhoAgreement,d_PValue,iEr, &
      l_SaveTest,da_STV,da_AvgCommDist)

USE domrspmod
implicit none
INTEGER :: i_NumObs,i_NumVars, i_NumPerms, iEr
REAL (KIND(0D0)) :: d_V
REAL (KIND(0D0)) :: d_T, d_DBar, d_D1, d_Var, d_Gam, d_Delta1,d_ExcessVal
REAL (KIND(0D0)) :: da_Data(i_NumObs,i_NumVars),da_AvgCommDist(i_NumVars)
REAL (KIND(0D0)) :: d_TestStat,d_ObsDelta,d_ExpDelta,d_VarDelta,d_SkwDelta,d_RhoAgreement,d_PValue
LOGICAL :: l_DoResample, l_DoEMRSP,l_IsCommens
REAL (KIND(0D0)) :: da_STV(i_NumPerms)  ! save test values
LOGICAL :: l_SaveTest
INTEGER :: i_seed
i_seed=-1

CALL runmrsp(da_Data,		&
				 d_V,    &
			     i_NumObs,		 &
				 i_NumVars,		 &
				 i_NumPerms,     &
				 l_DoEMRSP,		 &
				 l_DoResample,   &
				 l_IsCommens,    &
				 d_TestStat,     &
                 d_ObsDelta,     &
                 d_ExpDelta,     &
                 d_VarDelta,     &
                 d_SkwDelta,     &
                 d_RhoAgreement, &
                 d_PValue,       &
                 iEr,			 &
                 l_SaveTest,da_STV,i_seed,da_AvgCommDist)


END subroutine WrapMRSP

!===========================================================


!============================================================
subroutine WrapMEDQ(i_NumGrps, 		&
					i_NumVars,     &
					i_NumCases,    &
                    ia_GrpSizes,    &
                    da_Data,        &
                    i_NumQuantVals,   &
                    da_QuantVals, &
                    i_MaxGpSize,              &
                    da_VariableWInGpMedian,   &
                    da_GpAvgDistToGpMVMedian, &
                    da_GpMedQTolerance,       &
                    da_WInGrpVarEstimateVal,  &
                    da_WInGpQuantDist,        &
                    da_ObsDistToGpMedian,     &
                    ia_NumIterations,         &
                    iEr)


USE medqmod !might need to list all modules called by mrpp

implicit none

INTEGER :: i_NumCases,i_NumObvs, i_MaxGpSize,i_NumGrps, i_NumVars
INTEGER :: ia_GrpSizes(i_NumGrps),i_NumQuantVals,iEr,ia_NumIterations(i_NumGrps)
REAL (KIND(0D0)) :: da_GpVals(i_NumCases),da_Data(i_NumCases,i_NumVars),da_QuantVals(i_NumQuantVals)
REAL (KIND(0D0)) :: da_VariableWInGpMedian(i_NumGrps,i_NumVars),da_GpAvgDistToGpMVMedian(i_NumGrps)
REAL (KIND(0D0)) :: da_WInGpQuantDist(i_NumGrps,i_NumQuantVals),da_ObsDistToGpMedian(i_NumGrps,i_MaxGpSize)
REAL (KIND(0D0)) :: da_GpMedQTolerance(i_NumGrps),da_WInGrpVarEstimateVal(i_NumGrps,i_NumQuantVals)!

CALL runmedq(ia_GrpSizes,       &
                   i_NumQuantVals,  &
                   da_QuantVals,   &
                   i_NumGrps,     &
                   i_NumCases,      &
                   i_NumVars,       &
                   da_Data,                  &
                    da_VariableWInGpMedian,   &
                    da_GpAvgDistToGpMVMedian, &
                    da_GpMedQTolerance,       &
                    da_WInGrpVarEstimateVal,  &
                    da_WInGpQuantDist,        &
                    da_ObsDistToGpMedian,     &
                    ia_NumIterations,         &
                   da_GpVals,       &
                   iEr)

END subroutine WrapMEDQ

!==============================================================
subroutine WrapLad(da_Data,i_NumCases, i_NumVars, d_Theta, i_NumPermut, &
	i_NumToDrop, ia_PosToDrop, i_SaveTest, i_Test, &
	l_DoublePermutation, &
	d_To, d_Tn_RS, i_CntSumAVR, d_PValue, d_PVTn, &
	da_Betas, da_RedBetas, &
	d_SumAbsValRes, d_SumAbsValResRed, &
	d_WtSumAbsDevsFulMod, d_WtSumAbsDevsRedMod, &
	i_Iter, i_ExitCode, iEr,da_STV,i_LaHy,la_HasIntercept, i_DoAllQuants,            &
      l_DoLadTest, l_DoRankScore,l_IsOLS, l_HasHypCmdLine,l_IsLaPg,i_LaPgType,ia_NumLaVars,da_ResRed,da_Resids, &
      i_NumRows,i_NumCols,i_NumVarFull,i_NumVarRed,da_Sol,i_lsol)


USE DoLadMod
implicit none

!___Dummy Arguments:
INTEGER :: i_NumRows
INTEGER :: i_NumCols
INTEGER :: i_NumCases
INTEGER :: i_NumVars
REAL (KIND(0D0)) :: d_Theta
INTEGER :: i_NumPermut
INTEGER :: i_NumToDrop !eventually should be intent in only
INTEGER :: ia_PosToDrop((i_NumVarFull))
INTEGER :: i_SaveTest
INTEGER :: i_NumVarFull
INTEGER :: i_NumVarRed
LOGICAL :: l_DoublePermutation
INTEGER :: i_Test
REAL (KIND(0D0)) :: da_Data(i_NumRows,i_NumCols)
REAL (KIND(0D0)) :: d_To
REAL (KIND(0D0)) :: d_Tn_RS
INTEGER :: i_CntSumAVR
REAL (KIND(0D0)) :: d_PValue
REAL (KIND(0D0)) :: d_PVTn
REAL (KIND(0D0)) :: da_Betas(i_NumVarFull)
REAL (KIND(0D0)) :: da_RedBetas(i_NumVarRed)
REAL (KIND(0D0)) :: d_SumAbsValRes
REAL (KIND(0D0)) :: d_SumAbsValResRed
REAL (KIND(0D0)) :: d_WtSumAbsDevsFulMod
REAL (KIND(0D0)) :: d_WtSumAbsDevsRedMod
INTEGER :: i_Iter
INTEGER :: i_ExitCode
INTEGER :: iEr
INTEGER :: i_LaHy
INTEGER :: i_DoAllQuants
REAL (KIND(0D0)) :: da_STV(i_NumPermut*i_SaveTest)
LOGICAL :: la_HasIntercept(2)
LOGICAL :: l_DoLadTest
LOGICAL :: l_DoRankScore
LOGICAL :: l_IsOLS
LOGICAL :: l_HasHypCmdLine
LOGICAL :: l_IsLaPg
INTEGER :: i_LaPgType
INTEGER :: ia_NumLaVars(2)
REAL (KIND(0D0)) :: da_ResRed(i_NumCases)     ! (1:i_NumObs)
REAL (KIND(0D0)) :: da_Resids(i_NumCases)     ! (1:i_NumObs)
REAL (KIND(0D0)) :: da_Sol((i_NumVars+3)*i_DoAllQuants+1,(4*i_NumCases*i_DoAllQuants))

INTEGER :: i_lsol

CALL RunLad(da_Data,i_NumCases, i_NumVars, d_Theta, i_NumPermut, &
	i_NumToDrop, ia_PosToDrop, i_SaveTest, i_Test, &
	l_DoublePermutation, &
	d_To, d_Tn_RS, i_CntSumAVR, d_PValue, d_PVTn, &
	da_Betas, da_RedBetas, &
	d_SumAbsValRes, d_SumAbsValResRed, &
	d_WtSumAbsDevsFulMod, d_WtSumAbsDevsRedMod, &
	i_Iter, i_ExitCode, iEr,da_STV,i_LaHy,la_HasIntercept, i_DoAllQuants,            &
      l_DoLadTest, l_DoRankScore,l_IsOLS, l_HasHypCmdLine,l_IsLaPg,i_LaPgType,ia_NumLaVars,da_ResRed,da_Resids,da_Sol,i_lsol)


END subroutine WrapLAD


SUBROUTINE WrapCov(i_NumSimuls,         d_V_DistExp,         i_NumGrps,  &
                  ia_GrpSizes,         da_ObsData,          d_Del,       &
                  d_ET,                d_VT,                l_OkSDVT,   &
                  d_SDVT,              l_OkZ,               d_Z,        &
                  l_OkSKT,             d_SKT,               l_OkPVal,   &
                  d_PVal,              d_PZ,                iEr,        &
                  l_SaveTest,  da_STV, i_NumGroups,l_DoEGSECT, &
                  da_GpVals,i_NumObs,grpLength,d_Delta)
USE covermod
USE blcmnmod, ONLY: d_ZERO
IMPLICIT NONE
!___Dummy Aruguments:

INTEGER,          INTENT(INOUT)    :: i_NumSimuls
REAL (KIND(0D0)), INTENT(IN)    :: d_V_DistExp
INTEGER,          INTENT(IN)    :: i_NumGrps
INTEGER,          INTENT(INOUT)    :: ia_GrpSizes(grpLength)
REAL (KIND(0D0)), INTENT(INOUT) :: da_ObsData(i_NumObs)
REAL (KIND(0D0)), INTENT(OUT)   :: d_Del
REAL (KIND(0D0)), INTENT(OUT)   :: d_ET
REAL (KIND(0D0)), INTENT(OUT)   :: d_VT
LOGICAL,          INTENT(OUT)   :: l_OkSDVT
REAL (KIND(0D0)), INTENT(OUT)   :: d_SDVT
LOGICAL,          INTENT(OUT)   :: l_OkZ
REAL (KIND(0D0)), INTENT(OUT)   :: d_Z
LOGICAL,          INTENT(OUT)   :: l_OkSKT
REAL (KIND(0D0)), INTENT(OUT)   :: d_SKT
LOGICAL,          INTENT(OUT)   :: l_OkPVal
REAL (KIND(0D0)), INTENT(OUT)   :: d_PVal
REAL (KIND(0D0)), INTENT(OUT)   :: d_PZ
INTEGER,          INTENT(OUT)   :: iEr
LOGICAL, 		  INTENT(INOUT) :: l_SaveTest
REAL (KIND(0D0)), INTENT(OUT) :: da_STV(i_NumSimuls+1)   ! save test values
INTEGER,          INTENT(IN)    :: i_NumGroups
LOGICAL,          INTENT(IN)   :: l_DoEGSECT
REAL (KIND(0D0)), INTENT(IN)   :: da_GpVals(i_NumGrps)
INTEGER,          INTENT(IN)  :: i_NumObs
INTEGER,          INTENT(IN)  :: grpLength
REAL (KIND(0D0)), INTENT(OUT) :: d_Delta
d_Delta=d_ZERO
call rungsect(d_SKT, d_Delta, i_NumSimuls,d_V_DistExp,i_NumGrps,  &
                  ia_GrpSizes,da_ObsData, &
                  d_ET, d_VT,l_OkSDVT,   &
                  d_SDVT,l_OkZ,d_Z,        &
                  l_OkSKT,l_OkPVal,   &
                  d_PVal, d_PZ, iEr,        &
                  l_SaveTest,  da_STV ,i_NumGroups,l_DoEGSECT,da_GpVals)

d_Del=d_Delta                  
!call dblepr("d_Delta after rungsect",15,d_Delta,1)
END subroutine WrapCov
!=====================================================================================
!=====================================================================================

SUBROUTINE Wrapksgf(da_ObserV, &
          d_TObs,    &
          d_TEst,    &
          d_VarT,    &
          l_OkZ,     &
          d_Z,       &
          l_OkSkwT,  &
          d_SkwT,    &
          d_PValue,  &
          iEr,       &
          d_ArcIntv, &
          l_DoArc,  &
          i_NumCases)

 USE covermod
IMPLICIT NONE

!___Dummy Arguments:
REAL (KIND(0D0)), INTENT(INOUT) :: da_ObserV(i_NumCases)
REAL (KIND(0D0)), INTENT(OUT)   :: d_Tobs
REAL (KIND(0D0)), INTENT(OUT)   :: d_Test
REAL (KIND(0D0)), INTENT(OUT)   :: d_VarT
LOGICAL,          INTENT(OUT)   :: l_OkZ
REAL (KIND(0D0)), INTENT(OUT)   :: d_Z
LOGICAL,          INTENT(OUT)   :: l_OkSkwT
REAL (KIND(0D0)), INTENT(OUT)   :: d_SkwT
REAL (KIND(0D0)), INTENT(OUT)   :: d_PValue
INTEGER,          INTENT(OUT)   :: iEr
REAL (KIND(0D0)), INTENT(IN) :: d_ArcIntv
LOGICAL,          INTENT(IN)   :: l_DoArc
INTEGER,          INTENT(IN)   :: i_NumCases

call runksgf(da_ObserV, &
          d_TObs,    &
          d_TEst,    &
          d_VarT,    &
          l_OkZ,     &
          d_Z,       &
          l_OkSkwT,  &
          d_SkwT,    &
          d_PValue,  &
          iEr,       &
          d_ArcIntv, &
          l_DoArc,   &
          i_NumCases)

END subroutine Wrapksgf
!========================================================================================
