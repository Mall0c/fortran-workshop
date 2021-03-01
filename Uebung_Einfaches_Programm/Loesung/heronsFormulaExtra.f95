! ------------------------------------------------------
! Compute the area of a triangle using Heron's formula
! ------------------------------------------------------

PROGRAM  HeronFormulaExtra
   IMPLICIT  NONE

   INTEGER  :: choice, choicePyt  ! Choice of Formula
   REAL     :: a, b, c             ! three sides
   REAL     :: s                   ! half of perimeter
   REAL     :: Area                ! triangle area
   LOGICAL  :: Cond_1, Cond_2      ! two logical conditions


   WRITE(*,*) "Für den Satz des Pythagoras, bitte 1 eingeben"
   WRITE(*,*) "Für den Satz des Heron bitte, 2 eingeben"
   WRITE(*,*) "Programm abbrechen, bitte beliebige Taste druecken (außer 1,2)"
   READ(*,*) choice


   IF(choice == 2) THEN
   	WRITE(*,*) "Gebe bitte alle drei Seiten des Dreicks ein"

   	WRITE(*,*) "Bitte a eintragen"
   	READ(*,*)  a
   	WRITE(*,*)  "a = ", a

   	WRITE(*,*) "Bitte b eintragen"
   	READ(*,*) b
   	WRITE(*,*)  "b = ", b

   	WRITE(*,*) "Bitte c eintragen"   
   	READ(*,*) c
   	WRITE(*,*)  "c = ", c
   	WRITE(*,*) "--------------------------------------------------------------"
   	WRITE(*,*)

   	Cond_1 = (a > 0.) .AND. (b > 0.) .AND. (c > 0.0)
   	Cond_2 = (a + b > c) .AND. (a + c > b) .AND. (b + c > a)
   	IF (Cond_1 .AND. Cond_2) THEN
      		s    = (a + b + c) / 2.0
      		Area = SQRT(s * (s - a) * (s - b) * (s - c))
      	WRITE(*,*) "Der Flächenihalt des Dreiecks = ", Area
   	ELSE
      		WRITE(*,*) "ERROR: Das ist kein Dreieck!"
   		WRITE(*,*) "Programm beendet"
   	END IF
   END IF
  
   IF(choice .EQ. 1) THEN
	WRITE(*,*) "Um eine fehlende Kathete auszurechnen, bitte 0 eingeben"
	WRITE(*,*) "um eine fehlende Hypotenuse auszurechnen, bitte 1 eingeben"
	WRITE(*,*) "--------------------------------------------------------------"
    	READ(*,*) choicePyt

	IF(choicePyt .EQ. 0) THEN
       		WRITE(*,*) "Gebe die Länge der bekannten Kathete ein:"
		READ(*,*) a
		WRITE(*,*) "Gebe die Länge der Hypotenuse ein:"
		READ(*,*) c
		
		b = sqrt(c**2 - a**2)
		WRITE(*,*) "--------------------------------------------------------------"
		WRITE(*,*) "Die Länge der fehlende Kathete betraegt: ", b
	END If
	
	IF(choicePyt .EQ. 1) THEN
		WRITE(*,*) "Gebe die Länge der ersten Kathete ein:"
		READ(*,*) a
		WRITE(*,*) "Gebe die Länge der zweiten Kathete ein:"
		READ(*,*) b

		c = sqrt(a**2 + b**2)
		WRITE(*,*) "--------------------------------------------------------------"
		WRITE(*,*) "Die länge der Hypotenuse betraegt: ", c	
	END IF
	
	IF(choicePyt .NE. 0 .AND. choicePyt .NE. 1) THEN
		WRITE(*,*) "Falsche Eingabe"
   		WRITE(*,*) "Programm beendet"
	END IF
   END IF 
   
   IF(choice .NE. 1 .AND. choice .NE. 2) THEN
   	WRITE(*,*) "Eingabe ungueltig"
   	WRITE(*,*) "Programm beendet"
   END IF  

END PROGRAM  HeronFormulaExtra
