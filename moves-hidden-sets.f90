SUBROUTINE Hidden_sets(icage)

  USE global_variables
  IMPLICIT NONE

  INTEGER :: icage,ncells,icell,row1,col1
  INTEGER :: empty_cells,filled_cells,cage_cells
  INTEGER,DIMENSION(9) :: empty,filled
  INTEGER,DIMENSION(9) :: check_sudoku_logic
  INTEGER :: check_sudoku_logic_values
  INTEGER :: inumber,total_permutations,jcell
  INTEGER :: temp_number,jnumber
  INTEGER,DIMENSION(9) :: temp_array
  INTEGER :: check_numbers,cell_logic_number
  INTEGER :: i,j,kcell,lower_state
  INTEGER, pointer :: pa(:,:,:)
  INTEGER :: cage_sum_local
  INTEGER :: temp_cell_value,temp_row,temp_col
  INTEGER,DIMENSION(9) :: empty_row,empty_col
  INTEGER :: kvalue,knumber,check_cell
  INTEGER :: row2,col2
  INTEGER :: ncells1
  INTEGER,DIMENSION(naked_list_max,4,2) :: naked_list
  INTEGER,DIMENSION(9) :: naked_values
  INTEGER,DIMENSION(9) :: hidden_set
  INTEGER :: hidden_values,check_comb,count
  INTEGER :: inaked,jnaked,naked_cells
  INTEGER,DIMENSION(9) :: empty_values
  INTEGER,DIMENSION(max_grid_size1,max_grid_size2) :: naked_cells_all
  INTEGER :: cell_ij_numbers,cell_ij_logic_number 
  INTEGER :: check_value,ivalue 
  INTEGER :: cage1h_real,cage2h_real,check_cage1
  INTEGER :: jcage,iicage,jj


    ncells = cage_no_of_cells(icage)

    cage_cells = ncells
    cage_sum_local = cage_sum(icage)

     IF (cage_cells .eq. 2) THEN
        pa => cell_2(3:17,:,:)
        lower_state = 3
     ELSE IF (cage_cells .eq. 3) THEN
        pa => cell_3(6:24,:,:)
        lower_state = 6
     ELSE IF (cage_cells .eq. 4) THEN
        pa => cell_4(10:30,:,:)
        lower_state = 10
     ELSE IF (cage_cells .eq. 5) THEN
        pa => cell_5(15:35,:,:)
        lower_state = 15
     ELSE IF (cage_cells .eq. 6) THEN
        pa => cell_6(21:39,:,:)
        lower_state = 21
     ELSE IF (cage_cells .eq. 7) THEN
        pa => cell_7(28:42,:,:)
        lower_state = 28
     ELSE IF (cage_cells .eq. 8) THEN
        pa => cell_8(36:44,:,:)
        lower_state = 36
     ELSE IF (cage_cells .eq. 9) THEN
        pa => cell_9(45:45,:,:)
        lower_state = 45
     ELSE
        write(*,*) 'cage cells not between 2 and 9'
        STOP
     END IF

   empty_cells = 0
   filled_cells = 0
   check_sudoku_logic_values = 0
   check_sudoku_logic = 0
   filled = 0
   naked_list = 0
   naked_cells = 0
   naked_values = 0
   empty_values = 0  
   naked_cells_all=0

   CALL Naked_pairs(icage,naked_list,naked_values)

iloop:   DO icell=1,ncells
     row1 = cage_to_cells(icage,icell,1)
     col1 = cage_to_cells(icage,icell,2)
     DO inaked=1,naked_list_max
      DO jnaked=1,4
        IF ((row1 .eq. naked_list(inaked,jnaked,1)) .AND. &
                col1 .eq. naked_list(inaked,jnaked,2)) THEN
          naked_cells = naked_cells+1
 ! COM: newly added code
          naked_cells_all(row1,col1)=1
          CYCLE iloop
        END IF
       END DO
      END DO
     IF (sudoku(row1,col1) .LE. 0) THEN
        empty_cells = empty_cells+1
        empty_row(empty_cells) = row1
        empty_col(empty_cells) = col1
     END IF
     IF (sudoku(row1,col1) .GT. 0) THEN
        filled_cells = filled_cells+1
        filled(filled_cells) = sudoku(row1,col1)
     END IF
   END DO iloop

!  write(*,*) 'NAKED HERE--1 :naked cells is :',naked_cells
  naked_cells=0
  DO row1=1,max_grid_size1
    DO col1=1,max_grid_size2
      IF (naked_cells_all(row1,col1) .eq. 1) THEN
	naked_cells=naked_cells+1
      END IF
    END DO
  END DO
!  write(*,*) 'NAKED HERE--2 :naked cells is :',naked_cells

   IF (filled_cells+naked_cells .eq. ncells) THEN
	RETURN
   END IF

   IF (filled_cells+naked_cells .gt. ncells) THEN
        RETURN
   END IF

   IF (ncells .eq. 2) THEN
        total_permutations = 4
   ELSE IF (ncells .eq. 3) THEN
        total_permutations = 8
   ELSE IF (ncells .eq. 4) THEN
        total_permutations = 12
   ELSE IF (ncells .eq. 5) THEN
        total_permutations = 12
   ELSE IF (ncells .eq. 6) THEN
        total_permutations = 8
   ELSE IF (ncells .eq. 7) THEN
        total_permutations = 4
   ELSE IF (ncells .eq. 8) THEN
        total_permutations = 1
   ELSE IF (ncells .eq. 9) THEN
        total_permutations = 1
   END IF

 
  check_comb = 0
  ncells1 = ncells
inumber_loop:   DO inumber=1,total_permutations
          IF (pa(cage_sum_local-lower_state+1,inumber,1) .eq. 0) THEN
                EXIT
          END IF
          check_cell = 0
          count = 0
jloop:          DO jcell=1,ncells1
           DO icell=1,filled_cells
            IF(pa(cage_sum_local-lower_state+1,inumber,jcell) .eq. filled(icell)) THEN
                check_cell = check_cell + 1
                CYCLE jloop
            END IF
          END DO
          DO icell=1,9
           IF (naked_values(icell) .eq. 1) THEN
            IF(pa(cage_sum_local-lower_state+1,inumber,jcell) .eq. icell) THEN
                check_cell = check_cell + 1
                CYCLE jloop
            END IF
           END IF
          END DO
         END DO jloop
	 IF (check_cell .eq. filled_cells+naked_cells) THEN
	   check_comb = check_comb+1
           DO icell=1,ncells1
             jnumber = pa(cage_sum_local-lower_state+1,inumber,icell)
	     empty_values(jnumber) = empty_values(jnumber) + 1
	   END DO
	 END IF
	END DO inumber_loop


       hidden_set = 0
       hidden_values = 0
iloop1: DO i=1,9
         IF (empty_values(i) .eq. check_comb) THEN
	   DO icell=1,filled_cells
             IF(i .eq. filled(icell)) THEN
	      CYCLE iloop1
	     END IF
	   END DO
	   IF (naked_values(i) .eq. 1) THEN
	     CYCLE iloop1
	   END IF
           hidden_values = hidden_values+1
	   hidden_set(hidden_values) = i
	 END IF
        END DO iloop1

       CALL calculate_hidden_sets(icage,hidden_values,hidden_set)

       ! Check if we have some sure values that are present in only one cell
       temp_array=0
       DO icell=1,ncells
         row1=cage_to_cells(icage,icell,1)
	 col1=cage_to_cells(icage,icell,2)
         cell_ij_numbers=sudoku_logic_values(row1,col1)
         DO inumber=1,cell_ij_numbers
           cell_ij_logic_number=sudoku_logic(row1,col1,inumber)
	   temp_array(cell_ij_logic_number) = &
		temp_array(cell_ij_logic_number)+1
	 END DO
        END DO 
!	write(*,*) 'temp_array is :'
!  	write(*,*) temp_array
iloop2:        DO ivalue=1,hidden_values
          IF (temp_array(hidden_set(ivalue)) .eq. 1) THEN
	    check_value = hidden_set(ivalue)
!	    write(*,*) 'check_value is :',check_value
	    ! put that in sudoku 
	    DO icell=1,ncells
	      row1=cage_to_cells(icage,icell,1)
	      col1=cage_to_cells(icage,icell,2)
	      cell_ij_numbers=sudoku_logic_values(row1,col1)
	      DO inumber=1,cell_ij_numbers
		cell_ij_logic_number=sudoku_logic(row1,col1,inumber)
		IF (cell_ij_logic_number .eq. check_value) THEN
		  sudoku_logic_values(row1,col1)=0
		  sudoku(row1,col1)=check_value
!---------------------
  cage1h_real=cell_to_cage(row1,col1,1)
  cage2h_real=cell_to_cage(row1,col1,2)
  ! check if the cage is already present in the forcin_cage_stack
!  check_cage1=0
!  DO jcage=1,forcing_ncages_stack
!   iicage=forcing_loop_cage(jcage)
!   IF (iicage .eq. cage1h_real) THEN
!     check_cage1=1
!     EXIT
!   END IF
!  END DO
!  IF (check_cage1 .eq. 0) THEN
!    forcing_ncages_stack=forcing_ncages_stack+1
!    forcing_loop_cage(forcing_ncages_stack)=cage1h_real
!  END IF
!  check_cage1=0
!  DO jcage=1,forcing_ncages_stack
!   iicage=forcing_loop_cage(jcage)
!   IF (iicage .eq. cage2h_real) THEN
!     check_cage1=1
!     EXIT
!   END IF
!  END DO
!  IF (check_cage1 .eq. 0) THEN
!    forcing_ncages_stack=forcing_ncages_stack+1
!    forcing_loop_cage(forcing_ncages_stack)=cage2h_real
!  END IF

  check_cage1=0
  DO jj=1,forcing_ncages_stack(forcing_loop_stack_length)
    jcage=forcing_loop_cage(forcing_loop_stack_length,jj)
    IF (jcage .eq. cage1h_real) THEN
      check_cage1=1
      EXIT
    END IF
  END DO
  IF (check_cage1 .eq. 0) THEN
    forcing_ncages_stack(forcing_loop_stack_length) = &
        forcing_ncages_stack(forcing_loop_stack_length)+1
    forcing_loop_cage(forcing_loop_stack_length, &
        forcing_ncages_stack(forcing_loop_stack_length))=cage1h_real
  END IF

  check_cage1=0
  DO jj=1,forcing_ncages_stack(forcing_loop_stack_length)
    jcage=forcing_loop_cage(forcing_loop_stack_length,jj)
    IF (jcage .eq. cage2h_real) THEN
      check_cage1=1
      EXIT
    END IF
  END DO
  IF (check_cage1 .eq. 0) THEN
    forcing_ncages_stack(forcing_loop_stack_length) = &
        forcing_ncages_stack(forcing_loop_stack_length)+1
    forcing_loop_cage(forcing_loop_stack_length, &
        forcing_ncages_stack(forcing_loop_stack_length))=cage2h_real
  END IF


!--------------------

		  CYCLE iloop2
		END IF
	      END DO
            END DO
          END IF
        END DO iloop2

END SUBROUTINE Hidden_sets

!---------------------------------------------------

SUBROUTINE calculate_hidden_sets(icage,hidden_values,hidden_set)

  USE global_variables
  IMPLICIT NONE

  INTEGER :: icage,hidden_values
  INTEGER,DIMENSION(9) :: hidden_set
  INTEGER :: ncells,row1,col1,row2,col2,row3,col3
  INTEGER :: row4,col4,row5,col5

  INTEGER :: i,j,k,l,m,n,p
  INTEGER,DIMENSION(9,9) :: check_cell_numbers
  INTEGER,DIMENSION(9,9,9) :: check_cell
  INTEGER :: check,check_cell_numbers_ij
  INTEGER :: logic_number_ij_k,logic_number_il_m
  INTEGER :: logic_number_lj_m
  INTEGER :: logic_numbers_ij,logic_numbers_il,logic_numbers_lj
  INTEGER :: small_cell_ij_column,small_cell_ij_row
  INTEGER :: small_cell_il_column,small_cell_il_row
  INTEGER :: small_cell_lj_column,small_cell_lj_row
  INTEGER,DIMENSION(9) :: check_cell_logic_number
  INTEGER :: row_check,column_check
  INTEGER :: check_cell_numbers_ik,check_cell_numbers_kj
  INTEGER :: check_cell_number,check_number
  INTEGER :: check_same_number_cells
  INTEGER,DIMENSION(9) :: check_cell_number_new
  INTEGER :: i_row,jnumber,knumber,lnumber,mnumber,pnumber
  INTEGER :: cell_j_numbers,cell_k_numbers,cell_l_numbers,cell_m_numbers
  INTEGER :: quad_number,check_quad_number
  INTEGER :: cell_j,cell_k,cell_l,cell_m
  INTEGER :: cell_j_logic_number,cell_k_logic_number,cell_l_logic_number,cell_m_logic_number
  INTEGER :: cell_p_logic_number,cell_p_numbers
  INTEGER :: i_sudoku_logic_numbers
  INTEGER :: check_duple_number,check_triple_number,duple_number,triple_number
  INTEGER :: i_column,column_k,row_k,j_column
  INTEGER :: ii1,jj1,ii,jj,kk,ll,mm,nn
  INTEGER :: small_cell_row,small_cell_column
  INTEGER,DIMENSION(9) :: temp_array
  INTEGER :: cell_duple_number,cell_other_logic_number,cell_other_numbers
  INTEGER :: cell_quad_number,cell_triple_number,other_cell,temp_number
  INTEGER :: ii_other,jj_other,inumber
  INTEGER :: j_cell,k_cell,l_cell,m_cell,p_cell
  INTEGER :: check_duple_combination
  INTEGER :: check_triple_combination
  INTEGER :: check_quad_combination
  INTEGER :: ivalue


  ncells = cage_no_of_cells(icage)

    DO j_cell=1,ncells
      row1 = cage_to_cells(icage,j_cell,1)
      col1 = cage_to_cells(icage,j_cell,2) 
      IF (sudoku(row1,col1) .ne. 0) CYCLE

      DO k_cell=j_cell+1,ncells 
	row2 = cage_to_cells(icage,k_cell,1)
	col2 = cage_to_cells(icage,k_cell,2)
        IF (sudoku(row2,col2) .ne. 0) CYCLE

	check_cell_number_new = 0
        cell_j_numbers = sudoku_logic_values(row1,col1)
        cell_k_numbers = sudoku_logic_values(row2,col2)

        DO jnumber=1,cell_j_numbers
                cell_j_logic_number=sudoku_logic(row1,col1,jnumber)
                DO p_cell=1,ncells
                  IF (p_cell .eq. j_cell) CYCLE
		  row3 = cage_to_cells(icage,p_cell,1)
		  col3 = cage_to_cells(icage,p_cell,2)
                  cell_p_numbers = sudoku_logic_values(row3,col3)
                  DO pnumber=1,cell_p_numbers
                      cell_p_logic_number=sudoku_logic(row3,col3,pnumber)
                      IF (cell_j_logic_number .eq. cell_p_logic_number) THEN
                        IF (p_cell .eq. k_cell)  THEN
                           IF (check_cell_number_new(cell_j_logic_number) .eq. 0) THEN
                              check_cell_number_new(cell_j_logic_number) = 1
                           END IF
                        ELSE
                           check_cell_number_new(cell_j_logic_number) = 2
                        END IF

                      END IF
                  END DO
                END DO
          END DO

         DO knumber=1,cell_k_numbers
                cell_k_logic_number=sudoku_logic(row2,col2,knumber)
                DO p_cell=1,ncells
                  IF (p_cell .eq. k_cell) CYCLE
                  row3 = cage_to_cells(icage,p_cell,1)
                  col3 = cage_to_cells(icage,p_cell,2)
                  cell_p_numbers = sudoku_logic_values(row3,col3)
                DO pnumber=1,cell_p_numbers
                      cell_p_logic_number=sudoku_logic(row3,col3,pnumber)
                      IF (cell_k_logic_number .eq. cell_p_logic_number) THEN
                        IF (p_cell .eq. j_cell)  THEN
                           IF (check_cell_number_new(cell_k_logic_number) .eq. 0) THEN
                             check_cell_number_new(cell_k_logic_number) = 1
                           END IF
                        ELSE
                           check_cell_number_new(cell_k_logic_number) = 2
                        END IF
                      END IF
                  END DO
                END DO
          END DO

         check_duple_number = 0
         DO duple_number=1,9
                IF (check_cell_number_new(duple_number) .eq. 1) THEN
                        check_duple_number = check_duple_number + 1
                END IF
         END DO

	 check_duple_combination = 0
         IF (check_duple_number .eq. 2) THEN
	   DO ivalue=1,hidden_values
             IF(check_cell_number_new(hidden_set(ivalue)) .eq. 1) THEN
		check_duple_combination = check_duple_combination + 1
	     END IF
	   END DO
	   IF (check_duple_combination .eq. 2) THEN
                DO other_cell=1,ncells
                  temp_array = 0
                  temp_number = 0
          	  row3 = cage_to_cells(icage,other_cell,1)
           	  col3 = cage_to_cells(icage,other_cell,2)
                  IF (sudoku(row3,col3) .ne. 0) CYCLE
                  IF ((other_cell .ne. j_cell) .AND. (other_cell .ne. k_cell)) CYCLE
                  cell_other_numbers=sudoku_logic_values(row3,col3)
                  DO inumber=1,cell_other_numbers
                    cell_other_logic_number=sudoku_logic(row3,col3,inumber)
                    DO duple_number=1,9
                        cell_duple_number = check_cell_number_new(duple_number)
                        IF (cell_duple_number .eq. 1) THEN
                          IF (cell_other_logic_number .eq. duple_number) THEN
                                temp_number = temp_number + 1
                                temp_array(temp_number) = duple_number
                          END IF
                        END IF
                      END DO
                    END DO

                    DO inumber=1,sudoku_logic_values(row3,col3)
                      sudoku_logic(row3,col3,inumber) = 0
                    END DO
                    sudoku_logic_values(row3,col3) = temp_number
                    DO inumber=1,temp_number
                      sudoku_logic(row3,col3,inumber) = temp_array(inumber)
                    END DO
                  END DO

         END IF
       END IF

     END DO
    END DO

!   write(*,*) 'I AM HERE --3'
!   write(*,*) 'ncells is :',ncells

    DO j_cell=1,ncells
      row1 = cage_to_cells(icage,j_cell,1)
      col1 = cage_to_cells(icage,j_cell,2)
      IF (sudoku(row1,col1) .ne. 0) CYCLE

      DO k_cell=j_cell+1,ncells
        row2 = cage_to_cells(icage,k_cell,1)
        col2 = cage_to_cells(icage,k_cell,2)
        IF (sudoku(row2,col2) .ne. 0) CYCLE

	DO l_cell=k_cell+1,ncells
          row3 = cage_to_cells(icage,l_cell,1)
	  col3 = cage_to_cells(icage,l_cell,2)
	  IF (sudoku(row3,col3) .ne. 0) CYCLE

	check_cell_number_new = 0
        cell_j_numbers = sudoku_logic_values(row1,col1)
        cell_k_numbers = sudoku_logic_values(row2,col2)
        cell_l_numbers = sudoku_logic_values(row3,col3)

        DO jnumber=1,cell_j_numbers
                cell_j_logic_number=sudoku_logic(row1,col1,jnumber)
                DO p_cell=1,ncells
                  IF (p_cell .eq. j_cell) CYCLE
                  row4 = cage_to_cells(icage,p_cell,1)
                  col4 = cage_to_cells(icage,p_cell,2)
                  cell_p_numbers = sudoku_logic_values(row4,col4)
                  DO pnumber=1,cell_p_numbers
                      cell_p_logic_number=sudoku_logic(row4,col4,pnumber)
                      IF (cell_j_logic_number .eq. cell_p_logic_number) THEN
                        IF ((p_cell .eq. k_cell) .OR. (p_cell .eq. l_cell))  THEN
                           IF (check_cell_number_new(cell_j_logic_number) .eq. 0) THEN
                              check_cell_number_new(cell_j_logic_number) = 1
                           END IF
                        ELSE
                           check_cell_number_new(cell_j_logic_number) = 2
                        END IF

                      END IF
                  END DO
                END DO
          END DO

         DO knumber=1,cell_k_numbers
                cell_k_logic_number=sudoku_logic(row2,col2,knumber)
                DO p_cell=1,ncells
                  IF (p_cell .eq. k_cell) CYCLE
                  row4 = cage_to_cells(icage,p_cell,1)
                  col4 = cage_to_cells(icage,p_cell,2)
                  cell_p_numbers = sudoku_logic_values(row4,col4)
                DO pnumber=1,cell_p_numbers
                      cell_p_logic_number=sudoku_logic(row4,col4,pnumber)
                      IF (cell_k_logic_number .eq. cell_p_logic_number) THEN
                        IF ((p_cell .eq. j_cell) .OR. (p_cell .eq. l_cell))  THEN
                           IF (check_cell_number_new(cell_k_logic_number) .eq. 0) THEN
                             check_cell_number_new(cell_k_logic_number) = 1
                           END IF
                        ELSE
                           check_cell_number_new(cell_k_logic_number) = 2
                        END IF
                      END IF
                  END DO
                END DO
          END DO

         DO lnumber=1,cell_l_numbers
                cell_l_logic_number=sudoku_logic(row3,col3,lnumber)
                DO p_cell=1,ncells
                  IF (p_cell .eq. l_cell) CYCLE
		  row4 = cage_to_cells(icage,p_cell,1)
		  col4 = cage_to_cells(icage,p_cell,2)
                  cell_p_numbers = sudoku_logic_values(row4,col4)
                DO pnumber=1,cell_p_numbers
                      cell_p_logic_number=sudoku_logic(row4,col4,pnumber)
                      IF (cell_l_logic_number .eq. cell_p_logic_number) THEN
                        IF ((p_cell .eq. j_cell) .OR. (p_cell .eq. k_cell)) THEN
                           IF (check_cell_number_new(cell_l_logic_number) .eq. 0) THEN
                             check_cell_number_new(cell_l_logic_number) = 1
                           END IF
                        ELSE
                           check_cell_number_new(cell_l_logic_number) = 2
                        END IF
                      END IF
                  END DO
                END DO
          END DO

         check_triple_number = 0
         DO triple_number=1,9
                IF (check_cell_number_new(triple_number) .eq. 1) THEN
                        check_triple_number = check_triple_number + 1
                END IF
         END DO

         check_triple_combination = 0
!	 write(*,*) 'check_triple_number is :',check_triple_number
!	 write(*,*) check_cell_number_new(:)
         IF (check_triple_number .eq. 3) THEN
!		write(*,*) 'I AM HERE --1'
           DO ivalue=1,hidden_values
             IF(check_cell_number_new(hidden_set(ivalue)) .eq. 1) THEN
                check_triple_combination = check_triple_combination + 1
             END IF
           END DO
           IF (check_triple_combination .eq. 3) THEN
!		write(*,*) 'I AM HERE --2'
                DO other_cell=1,ncells
                  temp_array = 0
                  temp_number = 0
                  row4 = cage_to_cells(icage,other_cell,1)
                  col4 = cage_to_cells(icage,other_cell,2)
                  IF (sudoku(row4,col4) .ne. 0) CYCLE
                  IF ((other_cell .ne. j_cell) .AND. (other_cell .ne. k_cell) &
			.AND. (other_cell .ne. l_cell)) CYCLE
                  cell_other_numbers=sudoku_logic_values(row4,col4)
                  DO inumber=1,cell_other_numbers
                    cell_other_logic_number=sudoku_logic(row4,col4,inumber)
                    DO triple_number=1,9
                        cell_triple_number = check_cell_number_new(triple_number)
                        IF (cell_triple_number .eq. 1) THEN
                          IF (cell_other_logic_number .eq. triple_number) THEN
                                temp_number = temp_number + 1
                                temp_array(temp_number) = triple_number
                          END IF
                        END IF
                      END DO
                    END DO

                    DO inumber=1,sudoku_logic_values(row4,col4)
                      sudoku_logic(row4,col4,inumber) = 0
                    END DO
                    sudoku_logic_values(row4,col4) = temp_number
                    DO inumber=1,temp_number
                      sudoku_logic(row4,col4,inumber) = temp_array(inumber)
                    END DO
                  END DO

	  END IF
         END IF

      END DO
    END DO
  END DO

!   write(*,*) 'I AM HERE --3'
!   write(*,*) 'ncells is :',ncells

    DO j_cell=1,ncells
      row1 = cage_to_cells(icage,j_cell,1)
      col1 = cage_to_cells(icage,j_cell,2)
      IF (sudoku(row1,col1) .ne. 0) CYCLE

      DO k_cell=j_cell+1,ncells
        row2 = cage_to_cells(icage,k_cell,1)
        col2 = cage_to_cells(icage,k_cell,2)
        IF (sudoku(row2,col2) .ne. 0) CYCLE

        DO l_cell=k_cell+1,ncells
          row3 = cage_to_cells(icage,l_cell,1)
          col3 = cage_to_cells(icage,l_cell,2)
          IF (sudoku(row3,col3) .ne. 0) CYCLE
          
          DO m_cell=l_cell+1,ncells
            row4 = cage_to_cells(icage,m_cell,1)
	    col4 = cage_to_cells(icage,m_cell,2)
!   write(*,*) 'I AM HERE --3-1'

	check_cell_number_new = 0
        cell_j_numbers = sudoku_logic_values(row1,col1)
        cell_k_numbers = sudoku_logic_values(row2,col2)
        cell_l_numbers = sudoku_logic_values(row3,col3)
        cell_m_numbers = sudoku_logic_values(row4,col4)

        DO jnumber=1,cell_j_numbers
                cell_j_logic_number=sudoku_logic(row1,col1,jnumber)
                DO p_cell=1,ncells
                  IF (p_cell .eq. j_cell) CYCLE
                  row5 = cage_to_cells(icage,p_cell,1)
                  col5 = cage_to_cells(icage,p_cell,2)
                  cell_p_numbers = sudoku_logic_values(row5,col5)
                  DO pnumber=1,cell_p_numbers
                      cell_p_logic_number=sudoku_logic(row5,col5,pnumber)
                      IF (cell_j_logic_number .eq. cell_p_logic_number) THEN
                        IF ((p_cell .eq. k_cell) .OR. (p_cell .eq. l_cell) .OR. &
				(p_cell .eq. m_cell))  THEN
                           IF (check_cell_number_new(cell_j_logic_number) .eq. 0) THEN
                              check_cell_number_new(cell_j_logic_number) = 1
                           END IF
                        ELSE
                           check_cell_number_new(cell_j_logic_number) = 2
                        END IF

                      END IF
                  END DO
                END DO
          END DO
!   write(*,*) 'I AM HERE --3-2'

         DO knumber=1,cell_k_numbers
                cell_k_logic_number=sudoku_logic(row2,col2,knumber)
                DO p_cell=1,ncells
                  IF (p_cell .eq. k_cell) CYCLE
                  row5 = cage_to_cells(icage,p_cell,1)
                  col5 = cage_to_cells(icage,p_cell,2)
                  cell_p_numbers = sudoku_logic_values(row5,col5)
                DO pnumber=1,cell_p_numbers
                      cell_p_logic_number=sudoku_logic(row5,col5,pnumber)
                      IF (cell_k_logic_number .eq. cell_p_logic_number) THEN
                        IF ((p_cell .eq. j_cell) .OR. (p_cell .eq. l_cell) .OR. &
				(p_cell .eq. m_cell))  THEN
                           IF (check_cell_number_new(cell_k_logic_number) .eq. 0) THEN
                             check_cell_number_new(cell_k_logic_number) = 1
                           END IF
                        ELSE
                           check_cell_number_new(cell_k_logic_number) = 2
                        END IF
                      END IF
                  END DO
                END DO
          END DO
!   write(*,*) 'I AM HERE --3-3'

         DO lnumber=1,cell_l_numbers
                cell_l_logic_number=sudoku_logic(row3,col3,lnumber)
                DO p_cell=1,ncells
                  IF (p_cell .eq. l_cell) CYCLE
                  row5 = cage_to_cells(icage,p_cell,1)
                  col5 = cage_to_cells(icage,p_cell,2)
                  cell_p_numbers = sudoku_logic_values(row3,col3)
                DO pnumber=1,cell_p_numbers
                      cell_p_logic_number=sudoku_logic(row5,col5,pnumber)
                      IF (cell_l_logic_number .eq. cell_p_logic_number) THEN
                        IF ((p_cell .eq. j_cell) .OR. (p_cell .eq. k_cell) .OR. &
				(p_cell .eq. m_cell)) THEN
                           IF (check_cell_number_new(cell_l_logic_number) .eq. 0) THEN
                             check_cell_number_new(cell_l_logic_number) = 1
                           END IF
                        ELSE
                           check_cell_number_new(cell_l_logic_number) = 2
                        END IF
                      END IF
                  END DO
                END DO
          END DO

         DO mnumber=1,cell_m_numbers
                cell_m_logic_number=sudoku_logic(row4,col4,mnumber)
                DO p_cell=1,ncells
                  IF (p_cell .eq. m_cell) CYCLE
                  row5 = cage_to_cells(icage,p_cell,1)
                  col5 = cage_to_cells(icage,p_cell,2)
                  cell_p_numbers = sudoku_logic_values(row5,col5)
                DO pnumber=1,cell_p_numbers
                      cell_p_logic_number=sudoku_logic(row5,col5,pnumber)
                      IF (cell_m_logic_number .eq. cell_p_logic_number) THEN
                        IF ((p_cell .eq. j_cell) .OR. (p_cell .eq. k_cell) .OR. &
				(p_cell .eq. l_cell)) THEN
                           IF (check_cell_number_new(cell_m_logic_number) .eq. 0) THEN
                             check_cell_number_new(cell_m_logic_number) = 1
                           END IF
                        ELSE
                           check_cell_number_new(cell_m_logic_number) = 2
                        END IF
                      END IF
                  END DO
                END DO
          END DO

!  	GO TO 700

        check_quad_number = 0
         DO quad_number=1,9
                IF (check_cell_number_new(quad_number) .eq. 1) THEN
                        check_quad_number = check_quad_number + 1
                END IF
         END DO
         check_quad_combination = 0
         IF (check_quad_number .eq. 4) THEN
           DO ivalue=1,hidden_values
             IF(check_cell_number_new(hidden_set(ivalue)) .eq. 1) THEN
                check_quad_combination = check_quad_combination + 1
             END IF
           END DO
           IF (check_quad_combination .eq. 4) THEN
                DO other_cell=1,ncells
                  temp_array = 0
                  temp_number = 0
                  row5 = cage_to_cells(icage,other_cell,1)
                  col5 = cage_to_cells(icage,other_cell,2)
                  IF (sudoku(row5,col5) .ne. 0) CYCLE
                  IF ((other_cell .ne. j_cell) .AND. (other_cell .ne. k_cell) .AND. &
                        (other_cell .ne. l_cell) .AND. (other_cell .ne. m_cell)) CYCLE
                  cell_other_numbers=sudoku_logic_values(row5,col5)
                  DO inumber=1,cell_other_numbers
                    cell_other_logic_number=sudoku_logic(row5,col5,inumber)
                    DO quad_number=1,9
                        cell_quad_number = check_cell_number_new(quad_number)
                        IF (cell_quad_number .eq. 1) THEN
                          IF (cell_other_logic_number .eq. quad_number) THEN
                                temp_number = temp_number + 1
                                temp_array(temp_number) = quad_number
                          END IF
                        END IF
                      END DO
                    END DO

                    DO inumber=1,sudoku_logic_values(row5,col5)
                      sudoku_logic(row5,col5,inumber) = 0
                    END DO
                    sudoku_logic_values(row5,col5) = temp_number
                    DO inumber=1,temp_number
                      sudoku_logic(row5,col5,inumber) = temp_array(inumber)
                    END DO
                  END DO

          END IF
         END IF

!700 CONTINUE

       END DO
      END DO
     END DO
    END DO
	  
END SUBROUTINE calculate_hidden_sets

!-----------------------------------------------------
