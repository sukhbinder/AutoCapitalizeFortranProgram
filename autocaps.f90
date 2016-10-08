module keys
     integer,parameter :: nn=211
     character*20 keywords(nn),UKEYWORDS(nn)

      data keywords/ &
          'selected_real_kind',    &
          'selected_int_kind',    &
          'date_and_time',    &
          'endsubroutine',    &
          'random_number',    &
          'endinterface',    &
          'set_exponent',    &
          'system_clock',    &
          'allocatable',    &
          'dot_product',    &
          'endfunction',    &
          'maxexponent',    &
          'minexponent',    &
          'random_seed',    &
          'assignment',    &
          'associated',    &
          'deallocate',    &
          'endprogram',    &
          'selectcase',    &
          'subroutine',    &
          'allocated',    &
          'backspace',    &
          'character',    &
          'dimension',    &
          'elemental',    &
          'elsewhere',    &
          'endforall',    &
          'endmodule',    &
          'endselect',    &
          'interface',    &
          'intrinsic',    &
          'parameter',    &
          'precision',    &
          'procedure',    &
          'recursive',    &
          'rrspacing',    &
          'transpose',    &
          'allocate',    &
          'bit_size',    &
          'contains',    &
          'cpu_time',    &
          'endwhere',    &
          'exponent',    &
          'external',    &
          'fraction',    &
          'function',    &
          'implicit',    &
          'len_trim',    &
          'operator',    &
          'optional',    &
          'transfer',    &
          'continue',    &
          'adjustl',    &
          'adjustr',    &
          'ceiling',    &
          'complex',    &
          'default',    &
          'endfile',    &
          'endtype',    &
          'eoshift',    &
          'epsilon',    &
          '.false.',    &
          'inquire',    &
          'integer',    &
          'ishiftc',    &
          'logical',    &
          'nearest',    &
          'nullify',    &
          'pointer',    &
          'present',    &
          'private',    &
          'product',    &
          'program',    &
          'reshape',    &
          'spacing',    &
          'cshift',    &
          'digits',    &
          'elseif',    &
          'forall',    &
          'iachar',    &
          'intent',    &
          'ishift',    &
          'lbound',    &
          'matmul',    &
          'maxloc',    &
          'maxval',    &
          'minloc',    &
          'minval',    &
          'module',    &
          'modulo',    &
          'mvbits',    &
          '.neqv.',    &
          'public',    &
          'repeat',    &
          'result',    &
          'return',    &
          'rewind',    &
          'select',    &
          'spread',    &
          'target',    &
          '.true.',    &
          'ubound',    &
          'unpack',    &
          'verify',    &
          'double',    &
          'achar',    &
          'aimag',    &
          '.and.',    &
          'anint',    &
          'atan2',    &
          'btest',    &
          'close',    &
          'cmplx',    &
          'conjg',    &
          'count',    &
          'cycle',    &
          'dprod',    &
          'enddo',    &
          'endif',    &
          '.eqv.',    &
          'floor',    &
          'ibclr',    &
          'ibits',    &
          'ibset',    &
          'ichar',    &
          'index',    &
          'inout',    &
          '.lge.',    &
          '.lgt.',    &
          '.lle.',    &
          'log10',    &
          'merge',    &
          '.not.',    &
          'print',    &
          'radix',    &
          'range',    &
          'scale',    &
          'shape',    &
          'where',    &
          'write',    &
          'acos',    &
          'aint',    &
          'asin',    &
          'atan',    &
          'call',    &
          'case',    &
          'char',    &
          'cosh',    &
          'dble',    &
          'else',    &
          '.eq.',    &
          'exit',    &
          'file',    &
          '.ge.',    &
          'goto',    &
          '.gt.',    &
          'huge',    &
          'iand',    &
          'ieor',    &
          'kind',    &
          '.le.',    &
          '.lt.',    &
          '.ne.',    &
          'nint',    &
          'none',    &
          'null',    &
          'only',    &
          'open',    &
          '.or.',    &
          'pack',    &
          'pure',    &
          'read',    &
          'real',    &
          'save',    &
          'scan',    &
          'sign',    &
          'sinh',    &
          'size',    &
          'sqrt',    &
          'stop',    &
          'tanh',    &
          'then',    &
          'tiny',    &
          'trim',    &
          'type',    &
          'data',    &
          'abs',    &
          'all',    &
          'any',    &
          'cos',    &
          'dim',    &
          'end',    &
          'exp',    &
          'int',    &
          'ior',    &
          'len',    &
          'llt',    &
          'log',    &
          'max',    &
          'min',    &
          'mod',    &
          'out',    &
          'sin',    &
          'sum',    &
          'tan',    &
          'use',    &
          'do',    &
          'go',    &
          'if',    &
          'in',    &
          'to' /

      DATA UKEYWORDS/ &
          'SELECTED_REAL_KIND',    &
          'SELECTED_INT_KIND',    &
          'DATE_AND_TIME',    &
          'ENDSUBROUTINE',    &
          'RANDOM_NUMBER',    &
          'ENDINTERFACE',    &
          'SET_EXPONENT',    &
          'SYSTEM_CLOCK',    &
          'ALLOCATABLE',    &
          'DOT_PRODUCT',    &
          'ENDFUNCTION',    &
          'MAXEXPONENT',    &
          'MINEXPONENT',    &
          'RANDOM_SEED',    &
          'ASSIGNMENT',    &
          'ASSOCIATED',    &
          'DEALLOCATE',    &
          'ENDPROGRAM',    &
          'SELECTCASE',    &
          'SUBROUTINE',    &
          'ALLOCATED',    &
          'BACKSPACE',    &
          'CHARACTER',    &
          'DIMENSION',    &
          'ELEMENTAL',    &
          'ELSEWHERE',    &
          'ENDFORALL',    &
          'ENDMODULE',    &
          'ENDSELECT',    &
          'INTERFACE',    &
          'INTRINSIC',    &
          'PARAMETER',    &
          'PRECISION',    &
          'PROCEDURE',    &
          'RECURSIVE',    &
          'RRSPACING',    &
          'TRANSPOSE',    &
          'ALLOCATE',    &
          'BIT_SIZE',    &
          'CONTAINS',    &
          'CPU_TIME',    &
          'ENDWHERE',    &
          'EXPONENT',    &
          'EXTERNAL',    &
          'FRACTION',    &
          'FUNCTION',    &
          'IMPLICIT',    &
          'LEN_TRIM',    &
          'OPERATOR',    &
          'OPTIONAL',    &
          'TRANSFER',    &
          'CONTINUE',    &
          'ADJUSTL',    &
          'ADJUSTR',    &
          'CEILING',    &
          'COMPLEX',    &
          'DEFAULT',    &
          'ENDFILE',    &
          'ENDTYPE',    &
          'EOSHIFT',    &
          'EPSILON',    &
          '.FALSE.',    &
          'INQUIRE',    &
          'INTEGER',    &
          'ISHIFTC',    &
          'LOGICAL',    &
          'NEAREST',    &
          'NULLIFY',    &
          'POINTER',    &
          'PRESENT',    &
          'PRIVATE',    &
          'PRODUCT',    &
          'PROGRAM',    &
          'RESHAPE',    &
          'SPACING',    &
          'CSHIFT',    &
          'DIGITS',    &
          'ELSEIF',    &
          'FORALL',    &
          'IACHAR',    &
          'INTENT',    &
          'ISHIFT',    &
          'LBOUND',    &
          'MATMUL',    &
          'MAXLOC',    &
          'MAXVAL',    &
          'MINLOC',    &
          'MINVAL',    &
          'MODULE',    &
          'MODULO',    &
          'MVBITS',    &
          '.NEQV.',    &
          'PUBLIC',    &
          'REPEAT',    &
          'RESULT',    &
          'RETURN',    &
          'REWIND',    &
          'SELECT',    &
          'SPREAD',    &
          'TARGET',    &
          '.TRUE.',    &
          'UBOUND',    &
          'UNPACK',    &
          'VERIFY',    &
          'DOUBLE',    &
          'ACHAR',    &
          'AIMAG',    &
          '.AND.',    &
          'ANINT',    &
          'ATAN2',    &
          'BTEST',    &
          'CLOSE',    &
          'CMPLX',    &
          'CONJG',    &
          'COUNT',    &
          'CYCLE',    &
          'DPROD',    &
          'ENDDO',    &
          'ENDIF',    &
          '.EQV.',    &
          'FLOOR',    &
          'IBCLR',    &
          'IBITS',    &
          'IBSET',    &
          'ICHAR',    &
          'INDEX',    &
          'INOUT',    &
          '.LGE.',    &
          '.LGT.',    &
          '.LLE.',    &
          'LOG10',    &
          'MERGE',    &
          '.NOT.',    &
          'PRINT',    &
          'RADIX',    &
          'RANGE',    &
          'SCALE',    &
          'SHAPE',    &
          'WHERE',    &
          'WRITE',    &
          'ACOS',    &
          'AINT',    &
          'ASIN',    &
          'ATAN',    &
          'CALL',    &
          'CASE',    &
          'CHAR',    &
          'COSH',    &
          'DBLE',    &
          'ELSE',    &
          '.EQ.',    &
          'EXIT',    &
          'FILE',    &
          '.GE.',    &
          'GOTO',    &
          '.GT.',    &
          'HUGE',    &
          'IAND',    &
          'IEOR',    &
          'KIND',    &
          '.LE.',    &
          '.LT.',    &
          '.NE.',    &
          'NINT',    &
          'NONE',    &
          'NULL',    &
          'ONLY',    &
          'OPEN',    &
          '.OR.',    &
          'PACK',    &
          'PURE',    &
          'READ',    &
          'REAL',    &
          'SAVE',    &
          'SCAN',    &
          'SIGN',    &
          'SINH',    &
          'SIZE',    &
          'SQRT',    &
          'STOP',    &
          'TANH',    &
          'THEN',    &
          'TINY',    &
          'TRIM',    &
          'TYPE',    &
          'DATA',    &
          'ABS',    &
          'ALL',    &
          'ANY',    &
          'COS',    &
          'DIM',    &
          'END',    &
          'EXP',    &
          'INT',    &
          'IOR',    &
          'LEN',    &
          'LLT',    &
          'LOG',    &
          'MAX',    &
          'MIN',    &
          'MOD',    &
          'OUT',    &
          'SIN',    &
          'SUM',    &
          'TAN',    &
          'USE',    &
          'DO',    &
          'GO',    &
          'IF',    &
          'IN',    &
          'TO' /

     end module

     Program autocaptialize
     use keys
     implicit none
     character*80 srcfile,tarfile,text(10000),temp
     character(len=120) buffer
     integer reclen,i,iwhc,noarg,IARGC,istat

     if(noarg == 1) then
       CALL GETARG(1, buffer)
       read(buffer,'(a)') srcfile
     else
       write(*,'(a)',advance='no') "Enter Source File with Path: "
       read(*,'(a)')srcfile
     end if

     tarfile=trim(srcfile)//".out"

           open(12,file=trim(srcfile),status='old',iostat=istat)
           if(istat .ne. 0) then
              write(*,'(a)',advance='no') "Error in opening file ",trim(srcfile)
              go to 999
           end if
           open(22,file=trim(tarfile),iostat=istat)
           if(istat .ne. 0) then
              write(*,'(a)',advance='no') "Error in opening file ",trim(tarfile)
              go to 999
           end if
           reclen=0
           i=1
           do
             read(12,'(a)',end=100)text(i)
             i=i+1
           end do
 100 continue
        close(12)
        reclen=i-1

        do i=1,reclen
         call findcard(text(i),iwhc)
         write(22,'(a)')trim(text(i))
        end do
        close(22)
        write(*,'(a)',advance='no') Trim(tarfile), " written !"

 999    continue

     end

     subroutine findcard(txt,iw)
      use keys
      integer iw
      character*80 txt
      iw=0
!
! check comments
      if((txt(1:1) .eq. 'c') .or. &
         (txt(1:1) .eq. '*') .or. &
         (txt(1:1) .eq. '!') .or. &
         (txt(1:1) .eq. 'C')) then
        iw=0
        return
      end if
!
! return if length is 0
      j=len(txt)
      if(j .eq. 0) then
        iw=0
        return
      end if

      do i=1,nn
        k=index(txt,trim(keywords(i)))
        if(k .eq. 0) then

        else
           if(txt(k-1:k-1) .eq. " ") then
             ik=len_trim(keywords(i))
!             print *,k,keywords(i),i,trim(txt),ik
             txt(k:k+ik)=ukeywords(i)
           end if
        end if

      end do



      end