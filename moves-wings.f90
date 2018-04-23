! Find out that the number being looked for Simple_Xwing must exist for
! all the possible combinations available for both the cages (VERY IMP !!)

SUBROUTINE Simple_XWing

  USE global_variables
  IMPLICIT NONE
  
  INTEGER :: i_row1,i_row2,j_column,i
  INTEGER,DIMENSION(9) :: temp_array1,temp_array2,temp_array3
  INTEGER :: inumber,jnumber,temp_number1,temp_number2,temp_number3
  INTEGER :: cell_ij_numbers,cell_ij_logic_number
  INTEGER,DIMENSION(2) :: xwing1_column,xwing2_column
  INTEGER,DIMENSION(2) :: xwing1_row,xwing2_row
  INTEGER :: j_column1,j_column2,i_row,i_row3,j_column3
  INTEGER :: j,cage1h_real,ncells1,icell,row1,col1
  INTEGER :: row_temp1,col_temp1,cage_temp1,cage_temp2
  INTEGER :: cage2h_real,ncells2,jcell,row2,col2
  INTEGER :: vertical_cage11,vertical_cage12
  INTEGER :: vertical_cage21,vertical_cage22
  INTEGER :: vicell,vncells1,vrow1,vcol1
  INTEGER :: horizontal_cage11,horizontal_cage12
  INTEGER :: horizontal_cage21,horizontal_cage22 
  INTEGER :: check_number
  INTEGER,DIMENSION(9) :: temp_array_new
  INTEGER :: ncount

  DO i=1,nhorizontal_cages
    cage1h_real=horizontal_cages(i)
    ncells1=cage_no_of_cells(cage1h_real)
    temp_array1=0
    DO icell=1,ncells1
      row1=cage_to_cells(cage1h_real,icell,1)
      col1=cage_to_cells(cage1h_real,icell,2)
!      cage_temp1=cell_to_cage(row1,col1,1)
!      cage_temp2=cell_to_cage(row1,col1,2)
!      IF (cage_temp1 .eq. cage1h_real) THEN
!        vertical_cage11 = cage_temp2
!      ELSE IF (cage_temp2 .eq. cage1h_real) THEN
!        vertical_cage11 = cage_temp1
!      ELSE
!        write(*,*) 'SOMETHING IS wrong: Find_2permutation_horizontalcage -1'
!        STOP
!      END IF
       IF (sudoku(row1,col1) .ne. 0) CYCLE
       cell_ij_numbers=sudoku_logic_values(row1,col1)
       DO inumber=1,cell_ij_numbers
	 cell_ij_logic_number=sudoku_logic(row1,col1,inumber)
	 temp_array1(cell_ij_logic_number) = temp_array1(cell_ij_logic_number)+1 
       END DO
     END DO 
     DO jnumber=1,9
       temp_number1=0
       xwing1_column=0
       xwing1_row=0
       IF (temp_array1(jnumber) .eq. 2) THEN
!------------------------------------------------------------------------
!------- check that jnumber must exist for all possible combinations for
!------- this cage ------------------------------------------------------
	 check_number=0
	 ncount=0
	 temp_array_new=0
         CALL check_sure_number(cage1h_real,jnumber,check_number,ncount,temp_array_new)
!         IF (ncount .eq. 0) THEN
!           write(*,*) 'SOMETHING IS WRONG - check sure number'
!           STOP
!         END IF
	 IF (check_number .eq. 0) CYCLE
!------------------------------------------------------------------------
	 DO icell=1,ncells1
	   row1=cage_to_cells(cage1h_real,icell,1)
	   col1=cage_to_cells(cage1h_real,icell,2)
	   cell_ij_numbers=sudoku_logic_values(row1,col1)
	   DO inumber=1,cell_ij_numbers
	     cell_ij_logic_number=sudoku_logic(row1,col1,inumber)
	     IF (cell_ij_logic_number .eq. jnumber) THEN
	       temp_number1=temp_number1+1
	       xwing1_column(temp_number1)=col1	
	       xwing1_row(temp_number1)=row1
	     END IF
	   END DO
          END DO

          row_temp1=xwing1_row(1)
	  col_temp1=xwing1_column(1)
	  cage_temp1=cell_to_cage(row_temp1,col_temp1,1)
	  cage_temp2=cell_to_cage(row_temp1,col_temp1,2)
	  IF (cage_temp1 .eq. cage1h_real) THEN
	    vertical_cage11=cage_temp2
	  ELSE IF (cage_temp2 .eq. cage1h_real) THEN
	    vertical_cage11=cage_temp1
	  ELSE
	    write(*,*) 'SOMETHING IS wronge'
	    STOP
	  END IF

          row_temp1=xwing1_row(2)
          col_temp1=xwing1_column(2)
          cage_temp1=cell_to_cage(row_temp1,col_temp1,1)
          cage_temp2=cell_to_cage(row_temp1,col_temp1,2)
          IF (cage_temp1 .eq. cage1h_real) THEN
            vertical_cage12=cage_temp2
          ELSE IF (cage_temp2 .eq. cage1h_real) THEN
            vertical_cage12=cage_temp1
          ELSE
            write(*,*) 'SOMETHING IS wronge'
            STOP
          END IF

	  DO j=i+1,nhorizontal_cages
	    cage2h_real=horizontal_cages(j)
	    ncells2=cage_no_of_cells(cage2h_real)
	    temp_array2=0
	    DO jcell=1,ncells2
	      row2=cage_to_cells(cage2h_real,jcell,1)
	      col2=cage_to_cells(cage2h_real,jcell,2)
	      IF (sudoku(row2,col2) .ne. 0) CYCLE
	      cell_ij_numbers=sudoku_logic_values(row2,col2)
	      DO inumber=1,cell_ij_numbers
		cell_ij_logic_number=sudoku_logic(row2,col2,inumber)
		temp_array2(cell_ij_logic_number)=temp_array2(cell_ij_logic_number)+1
	      END DO
	    END DO

	    temp_number2=0
	    xwing2_column=0
	    IF (temp_array2(jnumber) .ne. 2) CYCLE
!------------------------------------------------------------------------
!------- check that jnumber must exist for all possible combinations for
!------- this cage ------------------------------------------------------
            check_number=0
	    ncount=0
	    temp_array_new=0
            CALL check_sure_number(cage2h_real,jnumber,check_number,ncount,temp_array_new)
!         IF (ncount .eq. 0) THEN
!           write(*,*) 'SOMETHING IS WRONG - check sure number'
!           STOP
!         END IF
            IF (check_number .eq. 0) CYCLE
!------------------------------------------------------------------------
	    DO jcell=1,ncells2
	      row2=cage_to_cells(cage2h_real,jcell,1)
	      col2=cage_to_cells(cage2h_real,jcell,2)
	      cell_ij_numbers=sudoku_logic_values(row2,col2)
	      DO inumber=1,cell_ij_numbers
		cell_ij_logic_number=sudoku_logic(row2,col2,inumber)
		IF (cell_ij_logic_number .eq. jnumber) THEN
		  temp_number2=temp_number2+1
		  xwing2_column(temp_number2)=col2
		  xwing2_row(temp_number2)=row2
		END IF
	      END DO
            END DO

            row_temp1=xwing2_row(1)
            col_temp1=xwing2_column(1)
            cage_temp1=cell_to_cage(row_temp1,col_temp1,1)
            cage_temp2=cell_to_cage(row_temp1,col_temp1,2)
            IF (cage_temp1 .eq. cage2h_real) THEN
              vertical_cage21=cage_temp2
            ELSE IF (cage_temp2 .eq. cage2h_real) THEN
              vertical_cage21=cage_temp1
            ELSE
              write(*,*) 'SOMETHING IS wrong'
              STOP
            END IF

            row_temp1=xwing2_row(2)
            col_temp1=xwing2_column(2)
            cage_temp1=cell_to_cage(row_temp1,col_temp1,1)
            cage_temp2=cell_to_cage(row_temp1,col_temp1,2)
            IF (cage_temp1 .eq. cage2h_real) THEN
              vertical_cage22=cage_temp2
            ELSE IF (cage_temp2 .eq. cage2h_real) THEN
              vertical_cage22=cage_temp1
            ELSE
              write(*,*) 'SOMETHING IS wrong'
              STOP
            END IF

! Check if two Xwing1_row and xwing2_row are the same for both the columns
            IF ((vertical_cage11 .eq. vertical_cage21) .AND. &
			(vertical_cage12 .eq. vertical_cage22)) THEN
	       ! We have simple Xwing.. Remove this number from other squares
	       ! of the 2 columns (vertical_cage11 and vertical_cage12)
	       vncells1=cage_no_of_cells(vertical_cage11)
	       DO vicell=1,vncells1
		 temp_number3=0
		 temp_array3=0
		 vrow1=cage_to_cells(vertical_cage11,vicell,1)
		 vcol1=cage_to_cells(vertical_cage11,vicell,2)
		 IF (sudoku(vrow1,vcol1) .ne. 0) CYCLE
		 IF (vrow1 .eq. xwing1_row(1)) CYCLE
		 IF (vrow1 .eq. xwing2_row(1)) CYCLE
		 cell_ij_numbers=sudoku_logic_values(vrow1,vcol1)
		 DO inumber=1,cell_ij_numbers
		   cell_ij_logic_number=sudoku_logic(vrow1,vcol1,inumber)
		   IF (cell_ij_logic_number .eq. jnumber) CYCLE
		   temp_number3=temp_number3+1
		   temp_array3(temp_number3)=cell_ij_logic_number
		 END DO
		 DO inumber=1,cell_ij_numbers
		   sudoku_logic(vrow1,vcol1,inumber)=0
		 END DO
		 sudoku_logic_values(vrow1,vcol1)=temp_number3
		 DO inumber=1,temp_number3
		   sudoku_logic(vrow1,vcol1,inumber)=temp_array3(inumber)
		 END DO
	       END DO
	      
               vncells1=cage_no_of_cells(vertical_cage12)
               DO vicell=1,vncells1
                 temp_number3=0
                 temp_array3=0
                 vrow1=cage_to_cells(vertical_cage12,vicell,1)
                 vcol1=cage_to_cells(vertical_cage12,vicell,2)
                 IF (sudoku(vrow1,vcol1) .ne. 0) CYCLE
                 IF (vrow1 .eq. xwing1_row(1)) CYCLE
                 IF (vrow1 .eq. xwing2_row(1)) CYCLE
                 cell_ij_numbers=sudoku_logic_values(vrow1,vcol1)
                 DO inumber=1,cell_ij_numbers
                   cell_ij_logic_number=sudoku_logic(vrow1,vcol1,inumber)
                   IF (cell_ij_logic_number .eq. jnumber) CYCLE
                   temp_number3=temp_number3+1
                   temp_array3(temp_number3)=cell_ij_logic_number
                 END DO
                 DO inumber=1,cell_ij_numbers
                   sudoku_logic(vrow1,vcol1,inumber)=0
                 END DO
                 sudoku_logic_values(vrow1,vcol1)=temp_number3
                 DO inumber=1,temp_number3
                   sudoku_logic(vrow1,vcol1,inumber)=temp_array3(inumber)
                 END DO
               END DO
	     END IF
 	   END DO
	  END IF
	 END DO
       END DO       

  DO i=1,nvertical_cages
    cage1h_real=vertical_cages(i)
    ncells1=cage_no_of_cells(cage1h_real)
    temp_array1=0
    DO icell=1,ncells1
      row1=cage_to_cells(cage1h_real,icell,1)
      col1=cage_to_cells(cage1h_real,icell,2)
!      cage_temp1=cell_to_cage(row1,col1,1)
!      cage_temp2=cell_to_cage(row1,col1,2)
!      IF (cage_temp1 .eq. cage1h_real) THEN
!        vertical_cage11 = cage_temp2
!      ELSE IF (cage_temp2 .eq. cage1h_real) THEN
!        vertical_cage11 = cage_temp1
!      ELSE
!        write(*,*) 'SOMETHING IS wrong: Find_2permutation_horizontalcage -1'
!        STOP
!      END IF
       IF (sudoku(row1,col1) .ne. 0) CYCLE
       cell_ij_numbers=sudoku_logic_values(row1,col1)
       DO inumber=1,cell_ij_numbers
         cell_ij_logic_number=sudoku_logic(row1,col1,inumber)
         temp_array1(cell_ij_logic_number) = temp_array1(cell_ij_logic_number)+1
       END DO
     END DO
     DO jnumber=1,9
       temp_number1=0
       xwing1_column=0
       xwing1_row=0
       IF (temp_array1(jnumber) .eq. 2) THEN
!------------------------------------------------------------------------
!------- check that jnumber must exist for all possible combinations for
!------- this cage ------------------------------------------------------
         check_number=0
	 ncount=0
	 temp_array_new=0
         CALL check_sure_number(cage1h_real,jnumber,check_number,ncount,temp_array_new)
!         IF (ncount .eq. 0) THEN
!           write(*,*) 'SOMETHING IS WRONG - check sure number'
!           STOP
!         END IF
         IF (check_number .eq. 0) CYCLE
!------------------------------------------------------------------------
         DO icell=1,ncells1
           row1=cage_to_cells(cage1h_real,icell,1)
           col1=cage_to_cells(cage1h_real,icell,2)
           cell_ij_numbers=sudoku_logic_values(row1,col1)
           DO inumber=1,cell_ij_numbers
             cell_ij_logic_number=sudoku_logic(row1,col1,inumber)
             IF (cell_ij_logic_number .eq. jnumber) THEN
               temp_number1=temp_number1+1
               xwing1_column(temp_number1)=col1
               xwing1_row(temp_number1)=row1
             END IF
           END DO
          END DO

          row_temp1=xwing1_row(1)
          col_temp1=xwing1_column(1)
          cage_temp1=cell_to_cage(row_temp1,col_temp1,1)
          cage_temp2=cell_to_cage(row_temp1,col_temp1,2)
          IF (cage_temp1 .eq. cage1h_real) THEN
            horizontal_cage11=cage_temp2
          ELSE IF (cage_temp2 .eq. cage1h_real) THEN
            horizontal_cage11=cage_temp1
          ELSE
            write(*,*) 'SOMETHING IS wronge'
            STOP
          END IF

          row_temp1=xwing1_row(2)
          col_temp1=xwing1_column(2)
          cage_temp1=cell_to_cage(row_temp1,col_temp1,1)
          cage_temp2=cell_to_cage(row_temp1,col_temp1,2)
          IF (cage_temp1 .eq. cage1h_real) THEN
            horizontal_cage12=cage_temp2
          ELSE IF (cage_temp2 .eq. cage1h_real) THEN
            horizontal_cage12=cage_temp1
          ELSE
            write(*,*) 'SOMETHING IS wronge'
            STOP
          END IF

          DO j=i+1,nvertical_cages
            cage2h_real=vertical_cages(j)
            ncells2=cage_no_of_cells(cage2h_real)
            temp_array2=0
            DO jcell=1,ncells2
              row2=cage_to_cells(cage2h_real,jcell,1)
              col2=cage_to_cells(cage2h_real,jcell,2)
              IF (sudoku(row2,col2) .ne. 0) CYCLE
              cell_ij_numbers=sudoku_logic_values(row2,col2)
              DO inumber=1,cell_ij_numbers
                cell_ij_logic_number=sudoku_logic(row2,col2,inumber)
                temp_array2(cell_ij_logic_number)=temp_array2(cell_ij_logic_number)+1
              END DO
            END DO

            temp_number2=0
            xwing2_column=0
            IF (temp_array2(jnumber) .ne. 2) CYCLE
!------------------------------------------------------------------------
!------- check that jnumber must exist for all possible combinations for
!------- this cage ------------------------------------------------------
            check_number=0
	    ncount=0
	    temp_array_new=0
            CALL check_sure_number(cage2h_real,jnumber,check_number,ncount,temp_array_new)
!         IF (ncount .eq. 0) THEN
!           write(*,*) 'SOMETHING IS WRONG - check sure number'
!           STOP
!         END IF
            IF (check_number .eq. 0) CYCLE
!------------------------------------------------------------------------
            DO jcell=1,ncells2
              row2=cage_to_cells(cage2h_real,jcell,1)
              col2=cage_to_cells(cage2h_real,jcell,2)
              cell_ij_numbers=sudoku_logic_values(row2,col2)
              DO inumber=1,cell_ij_numbers
                cell_ij_logic_number=sudoku_logic(row2,col2,inumber)
                IF (cell_ij_logic_number .eq. jnumber) THEN
                  temp_number2=temp_number2+1
                  xwing2_column(temp_number2)=col2
                  xwing2_row(temp_number2)=row2
                END IF
              END DO
            END DO

            row_temp1=xwing2_row(1)
            col_temp1=xwing2_column(1)
            cage_temp1=cell_to_cage(row_temp1,col_temp1,1)
            cage_temp2=cell_to_cage(row_temp1,col_temp1,2)
            IF (cage_temp1 .eq. cage2h_real) THEN
              horizontal_cage21=cage_temp2
            ELSE IF (cage_temp2 .eq. cage2h_real) THEN
              horizontal_cage21=cage_temp1
            ELSE
              write(*,*) 'SOMETHING IS wrong'
              STOP
            END IF

            row_temp1=xwing2_row(2)
            col_temp1=xwing2_column(2)
            cage_temp1=cell_to_cage(row_temp1,col_temp1,1)
            cage_temp2=cell_to_cage(row_temp1,col_temp1,2)
            IF (cage_temp1 .eq. cage2h_real) THEN
              horizontal_cage22=cage_temp2
            ELSE IF (cage_temp2 .eq. cage2h_real) THEN
              horizontal_cage22=cage_temp1
            ELSE
              write(*,*) 'SOMETHING IS wrong'
              STOP
            END IF

! Check if two Xwing1_row and xwing2_row are the same for both the columns
            IF ((horizontal_cage11 .eq. horizontal_cage21) .AND. &
                        (horizontal_cage12 .eq. horizontal_cage22)) THEN
               ! We have simple Xwing.. Remove this number from other squares
               ! of the 2 columns (vertical_cage11 and vertical_cage12)
               vncells1=cage_no_of_cells(horizontal_cage11)
               DO vicell=1,vncells1
                 temp_number3=0
                 temp_array3=0
                 vrow1=cage_to_cells(horizontal_cage11,vicell,1)
                 vcol1=cage_to_cells(horizontal_cage11,vicell,2)
                 IF (sudoku(vrow1,vcol1) .ne. 0) CYCLE
                 IF (vcol1 .eq. xwing1_column(1)) CYCLE
                 IF (vcol1 .eq. xwing2_column(1)) CYCLE
                 cell_ij_numbers=sudoku_logic_values(vrow1,vcol1)
                 DO inumber=1,cell_ij_numbers
                   cell_ij_logic_number=sudoku_logic(vrow1,vcol1,inumber)
                   IF (cell_ij_logic_number .eq. jnumber) CYCLE
                   temp_number3=temp_number3+1
                   temp_array3(temp_number3)=cell_ij_logic_number
                 END DO
                 DO inumber=1,cell_ij_numbers
                   sudoku_logic(vrow1,vcol1,inumber)=0
                 END DO
                 sudoku_logic_values(vrow1,vcol1)=temp_number3
                 DO inumber=1,temp_number3
                   sudoku_logic(vrow1,vcol1,inumber)=temp_array3(inumber)
                 END DO
               END DO
               vncells1=cage_no_of_cells(horizontal_cage12)
               DO vicell=1,vncells1
                 temp_number3=0
                 temp_array3=0
                 vrow1=cage_to_cells(horizontal_cage12,vicell,1)
                 vcol1=cage_to_cells(horizontal_cage12,vicell,2)
                 IF (sudoku(vrow1,vcol1) .ne. 0) CYCLE
                 IF (vcol1 .eq. xwing1_column(1)) CYCLE
                 IF (vcol1 .eq. xwing2_column(1)) CYCLE
                 cell_ij_numbers=sudoku_logic_values(vrow1,vcol1)
                 DO inumber=1,cell_ij_numbers
                   cell_ij_logic_number=sudoku_logic(vrow1,vcol1,inumber)
                   IF (cell_ij_logic_number .eq. jnumber) CYCLE
                   temp_number3=temp_number3+1
                   temp_array3(temp_number3)=cell_ij_logic_number
                 END DO
                 DO inumber=1,cell_ij_numbers
                   sudoku_logic(vrow1,vcol1,inumber)=0
                 END DO
                 sudoku_logic_values(vrow1,vcol1)=temp_number3
                 DO inumber=1,temp_number3
                   sudoku_logic(vrow1,vcol1,inumber)=temp_array3(inumber)
                 END DO
               END DO
             END IF
           END DO
          END IF
         END DO
       END DO
 

END SUBROUTINE Simple_XWing

!--------------------------------------------------------

SUBROUTINE check_sure_number(cage1h_real,jnumber,check_number,ncount,temp_array_new)

  USE global_variables
  IMPLICIT NONE

  INTEGER :: cage1h_real,jnumber,check_number
  INTEGER,DIMENSION(naked_list_max,4,2) :: naked_list
  INTEGER,DIMENSION(9) :: naked_values
  INTEGER, pointer :: pa(:,:,:)
  INTEGER :: lower_state,cell_value,cage_cells,cage_sum_local
  INTEGER :: check1,total_permutations
  INTEGER :: j_cell,k_cell
  INTEGER,DIMENSION(9) :: temp_check,temp_array_new
  INTEGER :: temp_check_count,ii_cell,inaked,ncount
  INTEGER :: rowk,colk,inumber


		  check_number=0
                  cage_cells=cage_no_of_cells(cage1h_real)
                  cage_sum_local=cage_sum(cage1h_real)

                  temp_check=0
                  temp_check_count=0
                  DO ii_cell=1,cage_cells
                     rowk=cage_to_cells(cage1h_real,ii_cell,1)
                     colk=cage_to_cells(cage1h_real,ii_cell,2)
                     IF (sudoku(rowk,colk) .gt. 0) THEN
                        temp_check(sudoku(rowk,colk))=1
                        temp_check_count=temp_check_count+1
                     END IF
                  END DO
!                  naked_list=0
!                  naked_values=0
!                  CALL Naked_pairs(cage1h_real,naked_list,naked_values)
!                  DO inaked=1,9
!                    IF (naked_values(inaked) .eq. 1) THEN
!                        temp_check(inaked) = 1
!                         temp_check_count=temp_check_count+1
!                    END IF
!                  END DO

     IF (cage_cells .eq. 2) THEN
        pa => cell_2(3:17,:,:)
        lower_state = 3
        total_permutations=4
     ELSE IF (cage_cells .eq. 3) THEN
        pa => cell_3(6:24,:,:)
        lower_state = 6
        total_permutations=8
     ELSE IF (cage_cells .eq. 4) THEN
        pa => cell_4(10:30,:,:)
        lower_state = 10
        total_permutations=12
     ELSE IF (cage_cells .eq. 5) THEN
        pa => cell_5(15:35,:,:)
        lower_state = 15
        total_permutations=12
     ELSE IF (cage_cells .eq. 6) THEN
        pa => cell_6(21:39,:,:)
        lower_state = 21
        total_permutations=8
     ELSE IF (cage_cells .eq. 7) THEN
        pa => cell_7(28:42,:,:)
        lower_state = 28
        total_permutations=4
     ELSE IF (cage_cells .eq. 8) THEN
        pa => cell_8(36:44,:,:)
        lower_state = 36
        total_permutations=1
     ELSE IF (cage_cells .eq. 9) THEN
        pa => cell_9(45:45,:,:)
        lower_state = 45
        total_permutations=1
     ELSE
        write(*,*) 'cage cells not between 2 and 9'
        STOP
     END IF

                 temp_array_new=0
                 ncount=0
                 DO inumber=1,total_permutations
                   IF (pa(cage_sum_local-lower_state+1,inumber,1) .eq. 0) THEN
                     EXIT
                   END IF
                   check1=0
                   DO k_cell=1,cage_cells
                     cell_value=pa(cage_sum_local-lower_state+1,inumber,k_cell)
                     IF (temp_check(cell_value) .eq. 1) THEN
                        check1=check1+1
                     END IF
                   END DO
!                   IF (temp_check_count .gt. 1) THEN
                   IF (temp_check_count .ge. 1) THEN
                    IF (check1 .eq. temp_check_count) THEN
                     ncount=ncount+1
                     DO j_cell=1,cage_cells
                       cell_value=pa(cage_sum_local-lower_state+1,inumber,j_cell)
                       temp_array_new(cell_value)=temp_array_new(cell_value)+1
                     END  DO
                    END IF
                   END IF
                 END DO

		IF (ncount .gt. 0) THEN
                 IF (temp_array_new(jnumber) .eq. ncount) THEN
		   check_number = 1
		 END IF
		END IF



END SUBROUTINE check_sure_number

!--------------------------------------------------------
