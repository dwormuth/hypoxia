      SUBROUTINE QQQANS(YES)
      LOGICAL YES
      CHARACTER*1 ANS,Y,Y1,N,N1
      DATA Y,Y1,N,N1 /'Y','y','N','n'/
      YES = .FALSE.
      READ (*,'(A)') ANS
      IF ((ANS .EQ. Y) .OR. (ANS .EQ. Y1)) THEN
       YES = .TRUE.
      ENDIF
      RETURN
      END
