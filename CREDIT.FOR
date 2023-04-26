      SUBROUTINE CREDIT
      LOGICAL YES
  107 FORMAT (///,20X,'*** HYPOXIA STUDY ***'//
     A/'   Hypoxia is a program that allows the examiner to study the'
     B/' physiological changes with which man responds to hypoxia.'
     C/' It was originally written by Gary Kessler while at UVM, and'
     D/' has since been modified by Dave Wormuth.'
     E//'   We hope that this program will provide a tool for the study'
     F/' and clinical management of several forms of hypoxia.'
     G///'  Gary Kessler                    David Wormuth'
C    H/'  13 Mallard Drive                7626 Collins Street'
C    I/'  Colchester, VT 05446            Lowville, NY 13367'
C    J/'  802-658-9138                    315-376-7453'
     K////' Press ENTER to continue...')
      WRITE (*,107)
      CALL QQQANS (YES)
      RETURN
      END
