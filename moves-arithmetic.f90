SUBROUTINE Crisscross_arithmetic

  USE global_variables
  IMPLICIT NONE

!  INTEGER,DIMENSION(max_2cages,2) :: horizontal_2cages
!  INTEGER,DIMENSION(max_3cages,3) :: horizontal_3cages
!  INTEGER,DIMENSION(max_4cages,4) :: horizontal_4cages
!  INTEGER,DIMENSION(max_2cages,2) :: vertical_2cages
!  INTEGER,DIMENSION(max_3cages,3) :: vertical_3cages
!  INTEGER,DIMENSION(max_4cages,4) :: vertical_4cages
!  INTEGER :: nhorizontal_2cages,nvertical_2cages
!  INTEGER :: nhorizontal_3cages,nvertical_3cages
!  INTEGER :: nhorizontal_4cages,nvertical_4cages
  INTEGER :: i,j,cage1h,cage2h,cage1h_real,cage2h_real
  INTEGER :: cage1v,cage2v,cage1v_real,cage2v_real
  INTEGER,DIMENSION(max_grid_size1,max_grid_size2,3) :: overlap,overlap1
  INTEGER :: overlapping_cells,nonoverlapping_cells_horizontal
  INTEGER :: nonoverlapping_cells_vertical,nonoverlapping_cells_total
  INTEGER,DIMENSION(non_overlap_cells_horizontal_max,2) :: non_overlap_cells_horizontal
  INTEGER,DIMENSION(non_overlap_cells_vertical_max,2) :: non_overlap_cells_vertical
  INTEGER :: sum_horizontal,sum_vertical,difference
  INTEGER :: row1,col1,row2,col2,cell_numbers1,cell_numbers2
  INTEGER :: cell_numbers3,cell_numbers4,row3,col3,row4,col4
  INTEGER :: npossible_combination
  INTEGER,DIMENSION(9) :: empty_values1,empty_values2
  INTEGER,DIMENSION(9) :: empty_values3,empty_values4
  INTEGER :: cell_logic_number1,cell_logic_number2
  INTEGER :: cell_logic_number3,cell_logic_number4
  INTEGER :: inumber,jnumber,ivalue,icell,ncells,jcell
  INTEGER :: knumber,lnumber
  INTEGER :: ii,jj
  INTEGER :: cage3h,cage4h,cage3h_real,cage4h_real
  INTEGER :: cage3v,cage4v,cage3v_real,cage4v_real
  INTEGER :: cageh,cageh_real,cagev,cagev_real,k
  INTEGER,DIMENSION(naked_list_max,4,2) :: naked_list
  INTEGER,DIMENSION(9) :: naked_values,nvalues
  INTEGER :: inaked,sum_naked,naked_count
  INTEGER :: cell_logic_number,irow,icol
  INTEGER :: sum_horizontal1

 

!  nhorizontal_2cages = 0
!  nhorizontal_3cages = 0
!  nhorizontal_4cages = 0
!  nvertical_2cages = 0
!  nvertical_3cages = 0
!  nvertical_4cages = 0

!  IF (nhorizontal_cages .ge. 2) THEN
!    CALL Find_2permutation_horizontalcage(nhorizontal_2cages,horizontal_2cages)
!  END IF
!  IF (nhorizontal_cages .ge. 3) THEN
!    CALL Find_3permutation_horizontalcage(nhorizontal_3cages,horizontal_3cages)
!  END IF
!  IF (nhorizontal_cages .ge. 4) THEN
!    CALL Find_4permutation_horizontalcage(nhorizontal_4cages,horizontal_4cages)
!  END IF

!  CALL Find_2permutation_verticalcage(nvertical_2cages,vertical_2cages)
!  CALL Find_3permutation_verticalcage(nvertical_3cages,vertical_3cages)
!  CALL Find_4permutation_verticalcage(nvertical_4cages,vertical_4cages) 

  ! permutation of horizontal and vertical cages..
  ! (2,2) (2,3) (2,4) (3,2) (3,3) (3,4) (4,2) (4,3) (4,4)
  ! (1,2) (1,3) (1,4) (2,1) (3,1) (4,1)

  ! (5,1) (5,2) (5,3) (5,4) (5,5) 
  ! (1,5) (2,5) (3,5) (4,5) 
  ! (6,1) (6,2) (6,3) (6,4) (6,5) (6,6)
  ! (1,6) (2,6) (3,6) (4,6) (5,6)
  ! (7,1) (7,2) (7,3) (7,4) (7,5) (7,6) (7,7)
  ! (1,7) (2,7) (3,7) (4,7) (5,7) (6,7) 
  ! (8,1) (8,2) (8,3) (8,4) (8,5) (8,6) (8,7) (8,8)
  ! (1,8) (2,8) (3,8) (4,8) (5,8) (6,8) (7,8)

  ! Do it for the following
  ! (3,5) (4,5) (5,5) (5,3) (5,4)
  ! (4,6) (5,6) (6,6) (6,5) ( 6,4)
  ! (5,7) (6,7) (7,7) (7,6) (7,5)
  ! (6,8) (7,8) (8,8) (8,7) (8,6)


  overlap = 0
  DO i=1,nhorizontal_cages
    cage1h_real = horizontal_cages(i)
    overlap = 0
    overlap1=0

    ncells = cage_no_of_cells(cage1h_real)
    DO icell=1,ncells
      row1 = cage_to_cells(cage1h_real,icell,1)
      col1 = cage_to_cells(cage1h_real,icell,2)
      overlap(row1,col1,1) = 1
    END DO
    overlap1=overlap

    DO j=1,nvertical_2cages
     cage1v = vertical_2cages(j,1)
     cage2v = vertical_2cages(j,2)
     cage1v_real = vertical_cages(cage1v)
     cage2v_real = vertical_cages(cage2v)
     overlap=overlap1
     overlap(:,:,2) = 0
     overlap(:,:,3) = 0

     ncells = cage_no_of_cells(cage1v_real)
     DO icell=1,ncells
       row2 = cage_to_cells(cage1v_real,icell,1)
       col2 = cage_to_cells(cage1v_real,icell,2)
       overlap(row2,col2,2) = 1
     END DO
     ncells = cage_no_of_cells(cage2v_real)
     DO icell=1,ncells
       row2 = cage_to_cells(cage2v_real,icell,1)
       col2 = cage_to_cells(cage2v_real,icell,2)
       overlap(row2,col2,2) = 1
     END DO

      sum_horizontal = cage_sum(cage1h_real) 
      sum_vertical = cage_sum(cage1v_real) + cage_sum(cage2v_real)

! Find all the nonoverlapping cells (that are 1 in horizontal and 0 in vertical and
!  vice versa) that have numbers in them.. make the overlap for these cells as 0 and
!  substract the number from the cage_sum..

  CALL number_in_cells_nonoverlap(overlap,sum_horizontal,sum_vertical)

! Similarly find the naked pairs, triples and quads that are in the non_overlapping cells
!  (all the cells should be non overlapping).. make the overlap for these cells as 0 and
!  susbtract the collective sum from the cage_sum
! It is possible that a naked list is duplicated.. so check if the overlap of the cell is 0 or 1..
!   we should remove the duplicate naked list from the cells.. also remove the numbers from
!   naked_values

   ! For horizontal cages
   DO k=1,1
     cageh_real=horizontal_cages(i)
     CALL naked_pairs_nonoverlap_horizontal(cageh_real,overlap,sum_horizontal)
   END DO

   ! For vertical cages
   DO k=1,2
     cageh=vertical_2cages(j,k)
     cageh_real=vertical_cages(cageh)
     CALL naked_pairs_nonoverlap_vertical(cageh_real,overlap,sum_vertical)
   END DO


      CALL calculate_crisscross_arithmetic(overlap,sum_horizontal, &
                                sum_vertical)

    END DO
  END DO


!----------------------------------------

  overlap = 0 
  DO i=1,nhorizontal_cages
    cage1h_real = horizontal_cages(i)
    overlap = 0
    overlap1=0

    ncells = cage_no_of_cells(cage1h_real)
    DO icell=1,ncells
      row1 = cage_to_cells(cage1h_real,icell,1)
      col1 = cage_to_cells(cage1h_real,icell,2)
      overlap(row1,col1,1) = 1
    END DO
    overlap1=overlap    

    DO j=1,nvertical_3cages 
     cage1v = vertical_3cages(j,1)
     cage2v = vertical_3cages(j,2)
     cage3v = vertical_3cages(j,3)
     cage1v_real = vertical_cages(cage1v)
     cage2v_real = vertical_cages(cage2v)
     cage3v_real = vertical_cages(cage3v)
     overlap=overlap1
     overlap(:,:,2) = 0
     overlap(:,:,3) = 0

     ncells = cage_no_of_cells(cage1v_real)
     DO icell=1,ncells
       row2 = cage_to_cells(cage1v_real,icell,1)
       col2 = cage_to_cells(cage1v_real,icell,2)
       overlap(row2,col2,2) = 1
     END DO
     ncells = cage_no_of_cells(cage2v_real)
     DO icell=1,ncells
       row2 = cage_to_cells(cage2v_real,icell,1)
       col2 = cage_to_cells(cage2v_real,icell,2)
       overlap(row2,col2,2) = 1
     END DO
     ncells = cage_no_of_cells(cage3v_real)
     DO icell=1,ncells
	row2 = cage_to_cells(cage3v_real,icell,1)
	col2 = cage_to_cells(cage3v_real,icell,2)
	overlap(row2,col2,2) = 1
     END DO

      sum_horizontal = cage_sum(cage1h_real)
      sum_vertical = cage_sum(cage1v_real) + cage_sum(cage2v_real) + &
			cage_sum(cage3v_real)

! Find all the nonoverlapping cells (that are 1 in horizontal and 0 in vertical and
!  vice versa) that have numbers in them.. make the overlap for these cells as 0 and
!  substract the number from the cage_sum..

  CALL number_in_cells_nonoverlap(overlap,sum_horizontal,sum_vertical)

! Similarly find the naked pairs, triples and quads that are in the non_overlapping cells
!  (all the cells should be non overlapping).. make the overlap for these cells as 0 and
!  susbtract the collective sum from the cage_sum
! It is possible that a naked list is duplicated.. so check if the overlap of the cell is 0 or 1..
!   we should remove the duplicate naked list from the cells.. also remove the numbers from
!   naked_values

   ! For horizontal cages
   DO k=1,1
     cageh_real=horizontal_cages(i)
     CALL naked_pairs_nonoverlap_horizontal(cageh_real,overlap,sum_horizontal)
   END DO

   ! For vertical cages
   DO k=1,3
     cageh=vertical_3cages(j,k)
     cageh_real=vertical_cages(cageh)
     CALL naked_pairs_nonoverlap_vertical(cageh_real,overlap,sum_vertical)
   END DO

      CALL calculate_crisscross_arithmetic(overlap,sum_horizontal, &
                                sum_vertical)

    END DO
  END DO

!-----------------------------------------
!GO TO 100
  overlap = 0
  DO i=1,nhorizontal_cages
    cage1h_real = horizontal_cages(i)
    overlap = 0
    overlap1=0

    ncells = cage_no_of_cells(cage1h_real)
    DO icell=1,ncells
      row1 = cage_to_cells(cage1h_real,icell,1)
      col1 = cage_to_cells(cage1h_real,icell,2)
      overlap(row1,col1,1) = 1
    END DO
    overlap1=overlap

    DO j=1,nvertical_4cages
     cage1v = vertical_4cages(j,1)
     cage2v = vertical_4cages(j,2)
     cage3v = vertical_4cages(j,3)
     cage4v = vertical_4cages(j,4)
     cage1v_real = vertical_cages(cage1v)
     cage2v_real = vertical_cages(cage2v)
     cage3v_real = vertical_cages(cage3v)
     cage4v_real = vertical_cages(cage4v)
     overlap=overlap1
     overlap(:,:,2) = 0
     overlap(:,:,3) = 0

     ncells = cage_no_of_cells(cage1v_real)
     DO icell=1,ncells
       row2 = cage_to_cells(cage1v_real,icell,1)
       col2 = cage_to_cells(cage1v_real,icell,2)
       overlap(row2,col2,2) = 1
     END DO
     ncells = cage_no_of_cells(cage2v_real)
     DO icell=1,ncells
       row2 = cage_to_cells(cage2v_real,icell,1)
       col2 = cage_to_cells(cage2v_real,icell,2)
       overlap(row2,col2,2) = 1
     END DO
     ncells = cage_no_of_cells(cage3v_real)
     DO icell=1,ncells
        row2 = cage_to_cells(cage3v_real,icell,1)
        col2 = cage_to_cells(cage3v_real,icell,2)
	overlap(row2,col2,2) = 1
     END DO
     ncells = cage_no_of_cells(cage4v_real)
     DO icell=1,ncells
	row2 = cage_to_cells(cage4v_real,icell,1)
	col2 = cage_to_cells(cage4v_real,icell,2)
	overlap(row2,col2,2) = 1
     END DO

      sum_horizontal = cage_sum(cage1h_real)
      sum_vertical = cage_sum(cage1v_real) + cage_sum(cage2v_real) + &
                        cage_sum(cage3v_real) + cage_sum(cage4v_real)

! Find all the nonoverlapping cells (that are 1 in horizontal and 0 in vertical and
!  vice versa) that have numbers in them.. make the overlap for these cells as 0 and
!  substract the number from the cage_sum..

  CALL number_in_cells_nonoverlap(overlap,sum_horizontal,sum_vertical)

! Similarly find the naked pairs, triples and quads that are in the non_overlapping cells
!  (all the cells should be non overlapping).. make the overlap for these cells as 0 and
!  susbtract the collective sum from the cage_sum
! It is possible that a naked list is duplicated.. so check if the overlap of the cell is 0 or 1..
!   we should remove the duplicate naked list from the cells.. also remove the numbers from
!   naked_values

   ! For horizontal cages
   DO k=1,1
     cageh_real=horizontal_cages(i)
     CALL naked_pairs_nonoverlap_horizontal(cageh_real,overlap,sum_horizontal)
   END DO

   ! For vertical cages
   DO k=1,4
     cageh=vertical_4cages(j,k)
     cageh_real=vertical_cages(cageh)
     CALL naked_pairs_nonoverlap_vertical(cageh_real,overlap,sum_vertical)
   END DO

      CALL calculate_crisscross_arithmetic(overlap,sum_horizontal, &
                                sum_vertical)

    END DO
  END DO

!100 CONTINUE

!-----------------------------------------
  overlap = 0
  DO i=1,nhorizontal_2cages
    cage1h = horizontal_2cages(i,1)
    cage2h = horizontal_2cages(i,2)
    cage1h_real = horizontal_cages(cage1h)
    cage2h_real = horizontal_cages(cage2h)
    overlap = 0
    overlap1=0

    ncells = cage_no_of_cells(cage1h_real)
    DO icell=1,ncells
      row1 = cage_to_cells(cage1h_real,icell,1)
      col1 = cage_to_cells(cage1h_real,icell,2)
      overlap(row1,col1,1) = 1
    END DO
    ncells = cage_no_of_cells(cage2h_real)
    DO icell=1,ncells
      row1 = cage_to_cells(cage2h_real,icell,1)
      col1 = cage_to_cells(cage2h_real,icell,2)
      overlap(row1,col1,1) = 1
    END DO
    overlap1=overlap

    DO j=1,nvertical_cages
     cage1v_real = vertical_cages(j)
     overlap=overlap1
     overlap(:,:,2) = 0
     overlap(:,:,3) = 0

     ncells = cage_no_of_cells(cage1v_real)
     DO icell=1,ncells
       row2 = cage_to_cells(cage1v_real,icell,1)
       col2 = cage_to_cells(cage1v_real,icell,2)
       overlap(row2,col2,2) = 1
     END DO

      sum_horizontal = cage_sum(cage1h_real) + cage_sum(cage2h_real)
      sum_vertical = cage_sum(cage1v_real) 

! Find all the nonoverlapping cells (that are 1 in horizontal and 0 in vertical and
!  vice versa) that have numbers in them.. make the overlap for these cells as 0 and
!  substract the number from the cage_sum..

  CALL number_in_cells_nonoverlap(overlap,sum_horizontal,sum_vertical)

! Similarly find the naked pairs, triples and quads that are in the non_overlapping cells
!  (all the cells should be non overlapping).. make the overlap for these cells as 0 and
!  susbtract the collective sum from the cage_sum
! It is possible that a naked list is duplicated.. so check if the overlap of the cell is 0 or 1..
!   we should remove the duplicate naked list from the cells.. also remove the numbers from
!   naked_values

   ! For horizontal cages
   DO k=1,2
     cageh=horizontal_2cages(i,k)
     cageh_real=horizontal_cages(cageh)
     CALL naked_pairs_nonoverlap_horizontal(cageh_real,overlap,sum_horizontal)
   END DO

   ! For vertical cages
   DO k=1,1
     cageh_real=vertical_cages(j)
     CALL naked_pairs_nonoverlap_vertical(cageh_real,overlap,sum_vertical)
   END DO

      CALL calculate_crisscross_arithmetic(overlap,sum_horizontal, &
                                sum_vertical)

    END DO
  END DO
  

!-----------------------------------------
  overlap = 0
  DO i=1,nhorizontal_3cages
    cage1h = horizontal_3cages(i,1)
    cage2h = horizontal_3cages(i,2)
    cage3h = horizontal_3cages(i,3)
    cage1h_real = horizontal_cages(cage1h)
    cage2h_real = horizontal_cages(cage2h)
    cage3h_real = horizontal_cages(cage3h)
    overlap = 0
    overlap1=0

    ncells = cage_no_of_cells(cage1h_real)
    DO icell=1,ncells
      row1 = cage_to_cells(cage1h_real,icell,1)
      col1 = cage_to_cells(cage1h_real,icell,2)
      overlap(row1,col1,1) = 1
    END DO
    ncells = cage_no_of_cells(cage2h_real)
    DO icell=1,ncells
      row1 = cage_to_cells(cage2h_real,icell,1)
      col1 = cage_to_cells(cage2h_real,icell,2)
      overlap(row1,col1,1) = 1
    END DO
    ncells = cage_no_of_cells(cage3h_real)
    DO icell=1,ncells
      row1 = cage_to_cells(cage3h_real,icell,1)
      col1 = cage_to_cells(cage3h_real,icell,2)
      overlap(row1,col1,1) = 1
    END DO
    overlap1=overlap	

    DO j=1,nvertical_cages
     cage1v_real = vertical_cages(j)
     overlap=overlap1
     overlap(:,:,2) = 0
     overlap(:,:,3) = 0

     ncells = cage_no_of_cells(cage1v_real)
     DO icell=1,ncells
       row2 = cage_to_cells(cage1v_real,icell,1)
       col2 = cage_to_cells(cage1v_real,icell,2)
       overlap(row2,col2,2) = 1
     END DO

      sum_horizontal = cage_sum(cage1h_real) + cage_sum(cage2h_real) + &
			cage_sum(cage3h_real)
      sum_vertical = cage_sum(cage1v_real)

! Find all the nonoverlapping cells (that are 1 in horizontal and 0 in vertical and
!  vice versa) that have numbers in them.. make the overlap for these cells as 0 and
!  substract the number from the cage_sum..

  CALL number_in_cells_nonoverlap(overlap,sum_horizontal,sum_vertical)

! Similarly find the naked pairs, triples and quads that are in the non_overlapping cells
!  (all the cells should be non overlapping).. make the overlap for these cells as 0 and
!  susbtract the collective sum from the cage_sum
! It is possible that a naked list is duplicated.. so check if the overlap of the cell is 0 or 1..
!   we should remove the duplicate naked list from the cells.. also remove the numbers from
!   naked_values

   ! For horizontal cages
   DO k=1,3
     cageh=horizontal_3cages(i,k)
     cageh_real=horizontal_cages(cageh)
     CALL naked_pairs_nonoverlap_horizontal(cageh_real,overlap,sum_horizontal)
   END DO

   ! For vertical cages
   DO k=1,1
     cageh_real=vertical_cages(j)
     CALL naked_pairs_nonoverlap_vertical(cageh_real,overlap,sum_vertical)
   END DO

      CALL calculate_crisscross_arithmetic(overlap,sum_horizontal, &
                                sum_vertical)

    END DO
  END DO

!  GO TO 200
!-----------------------------------------
  overlap = 0
  DO i=1,nhorizontal_4cages
    cage1h = horizontal_4cages(i,1)
    cage2h = horizontal_4cages(i,2)
    cage3h = horizontal_4cages(i,3)
    cage4h = horizontal_4cages(i,4)
    cage1h_real = horizontal_cages(cage1h)
    cage2h_real = horizontal_cages(cage2h)
    cage3h_real = horizontal_cages(cage3h)
    cage4h_real = horizontal_cages(cage4h)
    overlap = 0
    overlap1=0

    ncells = cage_no_of_cells(cage1h_real)
    DO icell=1,ncells
      row1 = cage_to_cells(cage1h_real,icell,1)
      col1 = cage_to_cells(cage1h_real,icell,2)
      overlap(row1,col1,1) = 1
    END DO
    ncells = cage_no_of_cells(cage2h_real)
    DO icell=1,ncells
      row1 = cage_to_cells(cage2h_real,icell,1)
      col1 = cage_to_cells(cage2h_real,icell,2)
      overlap(row1,col1,1) = 1
    END DO
    ncells = cage_no_of_cells(cage3h_real)
    DO icell=1,ncells
      row1 = cage_to_cells(cage3h_real,icell,1)
      col1 = cage_to_cells(cage3h_real,icell,2)
      overlap(row1,col1,1) = 1
    END DO
    ncells = cage_no_of_cells(cage4h_real)
    DO icell=1,ncells
      row1 = cage_to_cells(cage4h_real,icell,1)
      col1 = cage_to_cells(cage4h_real,icell,2)
      overlap(row1,col1,1) = 1
    END DO
    overlap1=overlap

    DO j=1,nvertical_cages
     cage1v_real = vertical_cages(j)
     overlap=overlap1
     overlap(:,:,2) = 0
     overlap(:,:,3) = 0

     ncells = cage_no_of_cells(cage1v_real)
     DO icell=1,ncells
       row2 = cage_to_cells(cage1v_real,icell,1)
       col2 = cage_to_cells(cage1v_real,icell,2)
       overlap(row2,col2,2) = 1
     END DO

      sum_horizontal = cage_sum(cage1h_real) + cage_sum(cage2h_real) + &
                        cage_sum(cage3h_real) + cage_sum(cage4h_real)
      sum_vertical = cage_sum(cage1v_real)

! Find all the nonoverlapping cells (that are 1 in horizontal and 0 in vertical and
!  vice versa) that have numbers in them.. make the overlap for these cells as 0 and
!  substract the number from the cage_sum..

  CALL number_in_cells_nonoverlap(overlap,sum_horizontal,sum_vertical)

! Similarly find the naked pairs, triples and quads that are in the non_overlapping cells
!  (all the cells should be non overlapping).. make the overlap for these cells as 0 and
!  susbtract the collective sum from the cage_sum
! It is possible that a naked list is duplicated.. so check if the overlap of the cell is 0 or 1..
!   we should remove the duplicate naked list from the cells.. also remove the numbers from
!   naked_values

   ! For horizontal cages
   DO k=1,4
     cageh=horizontal_4cages(i,k)
     cageh_real=horizontal_cages(cageh)
     CALL naked_pairs_nonoverlap_horizontal(cageh_real,overlap,sum_horizontal)
   END DO

   ! For vertical cages
   DO k=1,1
     cageh_real=vertical_cages(j)
     CALL naked_pairs_nonoverlap_vertical(cageh_real,overlap,sum_vertical)
   END DO

      CALL calculate_crisscross_arithmetic(overlap,sum_horizontal, &
                                sum_vertical)

    END DO
  END DO

!200 CONTINUE
!------------------------------------------
! (2,2) case

  overlap = 0
  DO i=1,nhorizontal_2cages
    cage1h = horizontal_2cages(i,1)
    cage2h = horizontal_2cages(i,2)
    cage1h_real = horizontal_cages(cage1h)
    cage2h_real = horizontal_cages(cage2h)
    overlap = 0
    overlap1=0

    ncells = cage_no_of_cells(cage1h_real)
    DO icell=1,ncells 
      row1 = cage_to_cells(cage1h_real,icell,1)
      col1 = cage_to_cells(cage1h_real,icell,2)
      overlap(row1,col1,1) = 1
    END DO
    ncells = cage_no_of_cells(cage2h_real)
    DO icell=1,ncells
      row1 = cage_to_cells(cage2h_real,icell,1)
      col1 = cage_to_cells(cage2h_real,icell,2)
      overlap(row1,col1,1) = 1
    END DO
    overlap1=overlap

    DO j=1,nvertical_2cages
     cage1v = vertical_2cages(j,1)
     cage2v = vertical_2cages(j,2)
     cage1v_real = vertical_cages(cage1v)
     cage2v_real = vertical_cages(cage2v)
     overlap=overlap1
     overlap(:,:,2) = 0
     overlap(:,:,3) = 0

     ncells = cage_no_of_cells(cage1v_real)
     DO icell=1,ncells
       row2 = cage_to_cells(cage1v_real,icell,1)
       col2 = cage_to_cells(cage1v_real,icell,2)
       overlap(row2,col2,2) = 1
     END DO
     ncells = cage_no_of_cells(cage2v_real)
     DO icell=1,ncells
       row2 = cage_to_cells(cage2v_real,icell,1)
       col2 = cage_to_cells(cage2v_real,icell,2)
       overlap(row2,col2,2) = 1
     END DO

      sum_horizontal = cage_sum(cage1h_real) + cage_sum(cage2h_real)
      sum_vertical = cage_sum(cage1v_real) + cage_sum(cage2v_real)

! Find all the nonoverlapping cells (that are 1 in horizontal and 0 in vertical and
!  vice versa) that have numbers in them.. make the overlap for these cells as 0 and
!  substract the number from the cage_sum..

  CALL number_in_cells_nonoverlap(overlap,sum_horizontal,sum_vertical)

! Similarly find the naked pairs, triples and quads that are in the non_overlapping cells
!  (all the cells should be non overlapping).. make the overlap for these cells as 0 and
!  susbtract the collective sum from the cage_sum
! It is possible that a naked list is duplicated.. so check if the overlap of the cell is 0 or 1..
!   we should remove the duplicate naked list from the cells.. also remove the numbers from
!   naked_values

   ! For horizontal cages
   DO k=1,2
     cageh=horizontal_2cages(i,k)
     cageh_real=horizontal_cages(cageh)
     CALL naked_pairs_nonoverlap_horizontal(cageh_real,overlap,sum_horizontal)
   END DO

   ! For vertical cages
   DO k=1,2
     cageh=vertical_2cages(j,k)
     cageh_real=vertical_cages(cageh)
     CALL naked_pairs_nonoverlap_vertical(cageh_real,overlap,sum_vertical)
   END DO
    
      CALL calculate_crisscross_arithmetic(overlap,sum_horizontal, &
				sum_vertical)

    END DO
  END DO

!------------------------------------------------------
  overlap = 0
  DO i=1,nhorizontal_2cages
    cage1h = horizontal_2cages(i,1)
    cage2h = horizontal_2cages(i,2)
    cage1h_real = horizontal_cages(cage1h)
    cage2h_real = horizontal_cages(cage2h)
    overlap = 0
    overlap1=0

    ncells = cage_no_of_cells(cage1h_real)
    DO icell=1,ncells
      row1 = cage_to_cells(cage1h_real,icell,1)
      col1 = cage_to_cells(cage1h_real,icell,2)
      overlap(row1,col1,1) = 1
    END DO
    ncells = cage_no_of_cells(cage2h_real)
    DO icell=1,ncells
      row1 = cage_to_cells(cage2h_real,icell,1)
      col1 = cage_to_cells(cage2h_real,icell,2)
      overlap(row1,col1,1) = 1
    END DO
    overlap1=overlap

    DO j=1,nvertical_3cages
     cage1v = vertical_3cages(j,1)
     cage2v = vertical_3cages(j,2)
     cage3v = vertical_3cages(j,3)
     cage1v_real = vertical_cages(cage1v)
     cage2v_real = vertical_cages(cage2v)
     cage3v_real = vertical_cages(cage3v)
     overlap=overlap1
     overlap(:,:,2) = 0
     overlap(:,:,3) = 0

     ncells = cage_no_of_cells(cage1v_real)
     DO icell=1,ncells
       row2 = cage_to_cells(cage1v_real,icell,1)
       col2 = cage_to_cells(cage1v_real,icell,2)
       overlap(row2,col2,2) = 1
     END DO
     ncells = cage_no_of_cells(cage2v_real)
     DO icell=1,ncells
       row2 = cage_to_cells(cage2v_real,icell,1)
       col2 = cage_to_cells(cage2v_real,icell,2)
       overlap(row2,col2,2) = 1
     END DO
     ncells = cage_no_of_cells(cage3v_real)
     DO icell=1,ncells
        row2 = cage_to_cells(cage3v_real,icell,1)
        col2 = cage_to_cells(cage3v_real,icell,2)
        overlap(row2,col2,2) = 1
     END DO

      sum_horizontal = cage_sum(cage1h_real) + cage_sum(cage2h_real)
      sum_vertical = cage_sum(cage1v_real) + cage_sum(cage2v_real) + &
                        cage_sum(cage3v_real)

! Find all the nonoverlapping cells (that are 1 in horizontal and 0 in vertical and
!  vice versa) that have numbers in them.. make the overlap for these cells as 0 and
!  substract the number from the cage_sum..

  CALL number_in_cells_nonoverlap(overlap,sum_horizontal,sum_vertical)

! Similarly find the naked pairs, triples and quads that are in the non_overlapping cells
!  (all the cells should be non overlapping).. make the overlap for these cells as 0 and
!  susbtract the collective sum from the cage_sum
! It is possible that a naked list is duplicated.. so check if the overlap of the cell is 0 or 1..
!   we should remove the duplicate naked list from the cells.. also remove the numbers from
!   naked_values

   ! For horizontal cages
   DO k=1,2
     cageh=horizontal_2cages(i,k)
     cageh_real=horizontal_cages(cageh)
     CALL naked_pairs_nonoverlap_horizontal(cageh_real,overlap,sum_horizontal)
   END DO

   ! For vertical cages
   DO k=1,3
     cageh=vertical_3cages(j,k)
     cageh_real=vertical_cages(cageh)
     CALL naked_pairs_nonoverlap_vertical(cageh_real,overlap,sum_vertical)
   END DO

      CALL calculate_crisscross_arithmetic(overlap,sum_horizontal, &
                                sum_vertical)

    END DO
  END DO

!  GO TO 300
!---------------------------------------------------
  overlap = 0
  DO i=1,nhorizontal_2cages
    cage1h = horizontal_2cages(i,1)
    cage2h = horizontal_2cages(i,2)
    cage1h_real = horizontal_cages(cage1h)
    cage2h_real = horizontal_cages(cage2h)
    overlap = 0
    overlap1=0

    ncells = cage_no_of_cells(cage1h_real)
    DO icell=1,ncells
      row1 = cage_to_cells(cage1h_real,icell,1)
      col1 = cage_to_cells(cage1h_real,icell,2)
      overlap(row1,col1,1) = 1
    END DO
    ncells = cage_no_of_cells(cage2h_real)
    DO icell=1,ncells
      row1 = cage_to_cells(cage2h_real,icell,1)
      col1 = cage_to_cells(cage2h_real,icell,2)
      overlap(row1,col1,1) = 1
    END DO
    overlap1=overlap

    DO j=1,nvertical_4cages
     cage1v = vertical_4cages(j,1)
     cage2v = vertical_4cages(j,2)
     cage3v = vertical_4cages(j,3)
     cage4v = vertical_4cages(j,4)
     cage1v_real = vertical_cages(cage1v)
     cage2v_real = vertical_cages(cage2v)
     cage3v_real = vertical_cages(cage3v)
     cage4v_real = vertical_cages(cage4v)
     overlap=overlap1
     overlap(:,:,2) = 0
     overlap(:,:,3) = 0

     ncells = cage_no_of_cells(cage1v_real)
     DO icell=1,ncells
       row2 = cage_to_cells(cage1v_real,icell,1)
       col2 = cage_to_cells(cage1v_real,icell,2)
       overlap(row2,col2,2) = 1
     END DO
     ncells = cage_no_of_cells(cage2v_real)
     DO icell=1,ncells
       row2 = cage_to_cells(cage2v_real,icell,1)
       col2 = cage_to_cells(cage2v_real,icell,2)
       overlap(row2,col2,2) = 1
     END DO
     ncells = cage_no_of_cells(cage3v_real)
     DO icell=1,ncells
        row2 = cage_to_cells(cage3v_real,icell,1)
        col2 = cage_to_cells(cage3v_real,icell,2)
        overlap(row2,col2,2) = 1
     END DO
     ncells = cage_no_of_cells(cage4v_real)
     DO icell=1,ncells
        row2 = cage_to_cells(cage4v_real,icell,1)
        col2 = cage_to_cells(cage4v_real,icell,2)
	overlap(row2,col2,2) = 1
     END DO

      sum_horizontal = cage_sum(cage1h_real) + cage_sum(cage2h_real)
      sum_vertical = cage_sum(cage1v_real) + cage_sum(cage2v_real) + &
                        cage_sum(cage3v_real) + cage_sum(cage4v_real)

! Find all the nonoverlapping cells (that are 1 in horizontal and 0 in vertical and
!  vice versa) that have numbers in them.. make the overlap for these cells as 0 and
!  substract the number from the cage_sum..

  CALL number_in_cells_nonoverlap(overlap,sum_horizontal,sum_vertical)

! Similarly find the naked pairs, triples and quads that are in the non_overlapping cells
!  (all the cells should be non overlapping).. make the overlap for these cells as 0 and
!  susbtract the collective sum from the cage_sum
! It is possible that a naked list is duplicated.. so check if the overlap of the cell is 0 or 1..
!   we should remove the duplicate naked list from the cells.. also remove the numbers from
!   naked_values

   ! For horizontal cages
   DO k=1,2
     cageh=horizontal_2cages(i,k)
     cageh_real=horizontal_cages(cageh)
     CALL naked_pairs_nonoverlap_horizontal(cageh_real,overlap,sum_horizontal)
   END DO

   ! For vertical cages
   DO k=1,4
     cageh=vertical_4cages(j,k)
     cageh_real=vertical_cages(cageh)
     CALL naked_pairs_nonoverlap_vertical(cageh_real,overlap,sum_vertical)
   END DO

      CALL calculate_crisscross_arithmetic(overlap,sum_horizontal, &
                                sum_vertical)

    END DO
  END DO

!   300 CONTINUE
!------------------------------------------------------

  overlap = 0
  DO i=1,nhorizontal_3cages
    cage1h = horizontal_3cages(i,1)
    cage2h = horizontal_3cages(i,2)
    cage3h = horizontal_3cages(i,3)
    cage1h_real = horizontal_cages(cage1h)
    cage2h_real = horizontal_cages(cage2h)
    cage3h_real = horizontal_cages(cage3h)
    overlap = 0
    overlap1=0

    ncells = cage_no_of_cells(cage1h_real)
    DO icell=1,ncells
      row1 = cage_to_cells(cage1h_real,icell,1)
      col1 = cage_to_cells(cage1h_real,icell,2)
      overlap(row1,col1,1) = 1
    END DO
    ncells = cage_no_of_cells(cage2h_real)
    DO icell=1,ncells
      row1 = cage_to_cells(cage2h_real,icell,1)
      col1 = cage_to_cells(cage2h_real,icell,2)
      overlap(row1,col1,1) = 1
    END DO
    ncells = cage_no_of_cells(cage3h_real)
    DO icell=1,ncells
      row1 = cage_to_cells(cage3h_real,icell,1)
      col1 = cage_to_cells(cage3h_real,icell,2)
      overlap(row1,col1,1) = 1
    END DO
    overlap1=overlap

    DO j=1,nvertical_2cages
     cage1v = vertical_2cages(j,1)
     cage2v = vertical_2cages(j,2)
     cage1v_real = vertical_cages(cage1v)
     cage2v_real = vertical_cages(cage2v)
     overlap=overlap1
     overlap(:,:,2) = 0
     overlap(:,:,3) = 0

     ncells = cage_no_of_cells(cage1v_real)
     DO icell=1,ncells
       row2 = cage_to_cells(cage1v_real,icell,1)
       col2 = cage_to_cells(cage1v_real,icell,2)
       overlap(row2,col2,2) = 1
     END DO
     ncells = cage_no_of_cells(cage2v_real)
     DO icell=1,ncells
       row2 = cage_to_cells(cage2v_real,icell,1)
       col2 = cage_to_cells(cage2v_real,icell,2)
       overlap(row2,col2,2) = 1
     END DO

      sum_horizontal = cage_sum(cage1h_real) + cage_sum(cage2h_real) + &
				cage_sum(cage3h_real)
      sum_vertical = cage_sum(cage1v_real) + cage_sum(cage2v_real)

! Find all the nonoverlapping cells (that are 1 in horizontal and 0 in vertical and
!  vice versa) that have numbers in them.. make the overlap for these cells as 0 and
!  substract the number from the cage_sum..

  CALL number_in_cells_nonoverlap(overlap,sum_horizontal,sum_vertical)

! Similarly find the naked pairs, triples and quads that are in the non_overlapping cells
!  (all the cells should be non overlapping).. make the overlap for these cells as 0 and
!  susbtract the collective sum from the cage_sum
! It is possible that a naked list is duplicated.. so check if the overlap of the cell is 0 or 1..
!   we should remove the duplicate naked list from the cells.. also remove the numbers from
!   naked_values

   ! For horizontal cages
   DO k=1,3
     cageh=horizontal_3cages(i,k)
     cageh_real=horizontal_cages(cageh)
     CALL naked_pairs_nonoverlap_horizontal(cageh_real,overlap,sum_horizontal)
   END DO

   ! For vertical cages
   DO k=1,2
     cageh=vertical_2cages(j,k)
     cageh_real=vertical_cages(cageh)
     CALL naked_pairs_nonoverlap_vertical(cageh_real,overlap,sum_vertical)
   END DO

      CALL calculate_crisscross_arithmetic(overlap,sum_horizontal, &
                                sum_vertical)

    END DO
  END DO

!----------------------------------------------------
  overlap=0
  DO i=1,nhorizontal_3cages
    cage1h = horizontal_3cages(i,1)
    cage2h = horizontal_3cages(i,2)
    cage3h = horizontal_3cages(i,3)
    cage1h_real = horizontal_cages(cage1h)
    cage2h_real = horizontal_cages(cage2h)
    cage3h_real = horizontal_cages(cage3h)
    overlap = 0
    overlap1=0

    ncells = cage_no_of_cells(cage1h_real)
    DO icell=1,ncells
      row1 = cage_to_cells(cage1h_real,icell,1)
      col1 = cage_to_cells(cage1h_real,icell,2)
      overlap(row1,col1,1) = 1
    END DO
    ncells = cage_no_of_cells(cage2h_real)
    DO icell=1,ncells
      row1 = cage_to_cells(cage2h_real,icell,1)
      col1 = cage_to_cells(cage2h_real,icell,2)
      overlap(row1,col1,1) = 1
    END DO
    ncells = cage_no_of_cells(cage3h_real)
    DO icell=1,ncells
      row1 = cage_to_cells(cage3h_real,icell,1)
      col1 = cage_to_cells(cage3h_real,icell,2)
      overlap(row1,col1,1) = 1
    END DO
    overlap1=overlap

    DO j=1,nvertical_3cages
     cage1v = vertical_3cages(j,1)
     cage2v = vertical_3cages(j,2)
     cage3v = vertical_3cages(j,3)
     cage1v_real = vertical_cages(cage1v)
     cage2v_real = vertical_cages(cage2v)
     cage3v_real = vertical_cages(cage3v)
     overlap=overlap1
     overlap(:,:,2) = 0
     overlap(:,:,3) = 0

     ncells = cage_no_of_cells(cage1v_real)
     DO icell=1,ncells
       row2 = cage_to_cells(cage1v_real,icell,1)
       col2 = cage_to_cells(cage1v_real,icell,2)
       overlap(row2,col2,2) = 1
     END DO
     ncells = cage_no_of_cells(cage2v_real)
     DO icell=1,ncells
       row2 = cage_to_cells(cage2v_real,icell,1)
       col2 = cage_to_cells(cage2v_real,icell,2)
       overlap(row2,col2,2) = 1
     END DO
     ncells = cage_no_of_cells(cage3v_real)
     DO icell=1,ncells
        row2 = cage_to_cells(cage3v_real,icell,1)
        col2 = cage_to_cells(cage3v_real,icell,2)
        overlap(row2,col2,2) = 1
     END DO

      sum_horizontal = cage_sum(cage1h_real) + cage_sum(cage2h_real) + &
			cage_sum(cage3h_real)
      sum_vertical = cage_sum(cage1v_real) + cage_sum(cage2v_real) + &
                        cage_sum(cage3v_real)

! Find all the nonoverlapping cells (that are 1 in horizontal and 0 in vertical and
!  vice versa) that have numbers in them.. make the overlap for these cells as 0 and
!  substract the number from the cage_sum..

  CALL number_in_cells_nonoverlap(overlap,sum_horizontal,sum_vertical)

! Similarly find the naked pairs, triples and quads that are in the non_overlapping cells
!  (all the cells should be non overlapping).. make the overlap for these cells as 0 and
!  susbtract the collective sum from the cage_sum
! It is possible that a naked list is duplicated.. so check if the overlap of the cell is 0 or 1..
!   we should remove the duplicate naked list from the cells.. also remove the numbers from
!   naked_values

   ! For horizontal cages
   DO k=1,3
     cageh=horizontal_3cages(i,k)
     cageh_real=horizontal_cages(cageh)
     CALL naked_pairs_nonoverlap_horizontal(cageh_real,overlap,sum_horizontal)
   END DO

   ! For vertical cages
   DO k=1,3
     cageh=vertical_3cages(j,k)
     cageh_real=vertical_cages(cageh)
     CALL naked_pairs_nonoverlap_vertical(cageh_real,overlap,sum_vertical)
   END DO

      CALL calculate_crisscross_arithmetic(overlap,sum_horizontal, &
                                sum_vertical)

    END DO
  END DO

!  GO TO 400
!----------------------------------------------------
  overlap = 0
  DO i=1,nhorizontal_3cages
    cage1h = horizontal_3cages(i,1)
    cage2h = horizontal_3cages(i,2)
    cage3h = horizontal_3cages(i,3)
    cage1h_real = horizontal_cages(cage1h)
    cage2h_real = horizontal_cages(cage2h)
    cage3h_real = horizontal_cages(cage3h)
    overlap = 0
    overlap1=0

    ncells = cage_no_of_cells(cage1h_real)
    DO icell=1,ncells
      row1 = cage_to_cells(cage1h_real,icell,1)
      col1 = cage_to_cells(cage1h_real,icell,2)
      overlap(row1,col1,1) = 1
    END DO
    ncells = cage_no_of_cells(cage2h_real)
    DO icell=1,ncells
      row1 = cage_to_cells(cage2h_real,icell,1)
      col1 = cage_to_cells(cage2h_real,icell,2)
      overlap(row1,col1,1) = 1
    END DO
    ncells = cage_no_of_cells(cage3h_real)
    DO icell=1,ncells
      row1 = cage_to_cells(cage3h_real,icell,1)
      col1 = cage_to_cells(cage3h_real,icell,2)
      overlap(row1,col1,1) = 1
    END DO
    overlap1=overlap

    DO j=1,nvertical_4cages
     cage1v = vertical_4cages(j,1)
     cage2v = vertical_4cages(j,2)
     cage3v = vertical_4cages(j,3)
     cage4v = vertical_4cages(j,4)
     cage1v_real = vertical_cages(cage1v)
     cage2v_real = vertical_cages(cage2v)
     cage3v_real = vertical_cages(cage3v)
     cage4v_real = vertical_cages(cage4v)
     overlap=overlap1
     overlap(:,:,2) = 0
     overlap(:,:,3) = 0

     ncells = cage_no_of_cells(cage1v_real)
     DO icell=1,ncells
       row2 = cage_to_cells(cage1v_real,icell,1)
       col2 = cage_to_cells(cage1v_real,icell,2)
       overlap(row2,col2,2) = 1
     END DO
     ncells = cage_no_of_cells(cage2v_real)
     DO icell=1,ncells
       row2 = cage_to_cells(cage2v_real,icell,1)
       col2 = cage_to_cells(cage2v_real,icell,2)
       overlap(row2,col2,2) = 1
     END DO
     ncells = cage_no_of_cells(cage3v_real)
     DO icell=1,ncells
        row2 = cage_to_cells(cage3v_real,icell,1)
        col2 = cage_to_cells(cage3v_real,icell,2)
        overlap(row2,col2,2) = 1
     END DO
     ncells = cage_no_of_cells(cage4v_real)
     DO icell=1,ncells
        row2 = cage_to_cells(cage4v_real,icell,1)
        col2 = cage_to_cells(cage4v_real,icell,2)
	overlap(row2,col2,2) = 1
     END DO

      sum_horizontal = cage_sum(cage1h_real) + cage_sum(cage2h_real) + &
			cage_sum(cage3h_real)
      sum_vertical = cage_sum(cage1v_real) + cage_sum(cage2v_real) + &
                        cage_sum(cage3v_real) + cage_sum(cage4v_real)

! Find all the nonoverlapping cells (that are 1 in horizontal and 0 in vertical and
!  vice versa) that have numbers in them.. make the overlap for these cells as 0 and
!  substract the number from the cage_sum..

  CALL number_in_cells_nonoverlap(overlap,sum_horizontal,sum_vertical)

! Similarly find the naked pairs, triples and quads that are in the non_overlapping cells
!  (all the cells should be non overlapping).. make the overlap for these cells as 0 and
!  susbtract the collective sum from the cage_sum
! It is possible that a naked list is duplicated.. so check if the overlap of the cell is 0 or 1..
!   we should remove the duplicate naked list from the cells.. also remove the numbers from
!   naked_values

   ! For horizontal cages
   DO k=1,3
     cageh=horizontal_3cages(i,k)
     cageh_real=horizontal_cages(cageh)
     CALL naked_pairs_nonoverlap_horizontal(cageh_real,overlap,sum_horizontal)
   END DO

   ! For vertical cages
   DO k=1,4
     cageh=vertical_4cages(j,k)
     cageh_real=vertical_cages(cageh)
     CALL naked_pairs_nonoverlap_vertical(cageh_real,overlap,sum_vertical)
   END DO

      CALL calculate_crisscross_arithmetic(overlap,sum_horizontal, &
                                sum_vertical)

    END DO
  END DO

! 400 CONTINUE

! GO TO 500
!-----------------------------------------------------
  overlap = 0
  DO i=1,nhorizontal_4cages
    cage1h = horizontal_4cages(i,1)
    cage2h = horizontal_4cages(i,2)
    cage3h = horizontal_4cages(i,3)
    cage4h = horizontal_4cages(i,4)
    cage1h_real = horizontal_cages(cage1h)
    cage2h_real = horizontal_cages(cage2h)
    cage3h_real = horizontal_cages(cage3h)
    cage4h_real = horizontal_cages(cage4h)
    overlap = 0
    overlap1=0

    ncells = cage_no_of_cells(cage1h_real)
    DO icell=1,ncells
      row1 = cage_to_cells(cage1h_real,icell,1)
      col1 = cage_to_cells(cage1h_real,icell,2)
      overlap(row1,col1,1) = 1
    END DO
    ncells = cage_no_of_cells(cage2h_real)
    DO icell=1,ncells
      row1 = cage_to_cells(cage2h_real,icell,1)
      col1 = cage_to_cells(cage2h_real,icell,2)
      overlap(row1,col1,1) = 1
    END DO
    ncells = cage_no_of_cells(cage3h_real)
    DO icell=1,ncells
      row1 = cage_to_cells(cage3h_real,icell,1)
      col1 = cage_to_cells(cage3h_real,icell,2)
      overlap(row1,col1,1) = 1
    END DO
    ncells = cage_no_of_cells(cage4h_real)
    DO icell=1,ncells
      row1 = cage_to_cells(cage4h_real,icell,1)
      col1 = cage_to_cells(cage4h_real,icell,2)
      overlap(row1,col1,1) = 1
    END DO
    overlap1=overlap

    DO j=1,nvertical_2cages
     cage1v = vertical_2cages(j,1)
     cage2v = vertical_2cages(j,2)
     cage1v_real = vertical_cages(cage1v)
     cage2v_real = vertical_cages(cage2v)
     overlap=overlap1
     overlap(:,:,2) = 0
     overlap(:,:,3) = 0

     ncells = cage_no_of_cells(cage1v_real)
     DO icell=1,ncells
       row2 = cage_to_cells(cage1v_real,icell,1)
       col2 = cage_to_cells(cage1v_real,icell,2)
       overlap(row2,col2,2) = 1
     END DO
     ncells = cage_no_of_cells(cage2v_real)
     DO icell=1,ncells
       row2 = cage_to_cells(cage2v_real,icell,1)
       col2 = cage_to_cells(cage2v_real,icell,2)
       overlap(row2,col2,2) = 1
     END DO

      sum_horizontal = cage_sum(cage1h_real) + cage_sum(cage2h_real) + &
			cage_sum(cage3h_real) + cage_sum(cage4h_real)
      sum_vertical = cage_sum(cage1v_real) + cage_sum(cage2v_real)

! Find all the nonoverlapping cells (that are 1 in horizontal and 0 in vertical and
!  vice versa) that have numbers in them.. make the overlap for these cells as 0 and
!  substract the number from the cage_sum..

  CALL number_in_cells_nonoverlap(overlap,sum_horizontal,sum_vertical)

! Similarly find the naked pairs, triples and quads that are in the non_overlapping cells
!  (all the cells should be non overlapping).. make the overlap for these cells as 0 and
!  susbtract the collective sum from the cage_sum
! It is possible that a naked list is duplicated.. so check if the overlap of the cell is 0 or 1..
!   we should remove the duplicate naked list from the cells.. also remove the numbers from
!   naked_values

   ! For horizontal cages
   DO k=1,4
     cageh=horizontal_4cages(i,k)
     cageh_real=horizontal_cages(cageh)
     CALL naked_pairs_nonoverlap_horizontal(cageh_real,overlap,sum_horizontal)
   END DO

   ! For vertical cages
   DO k=1,2
     cageh=vertical_2cages(j,k)
     cageh_real=vertical_cages(cageh)
     CALL naked_pairs_nonoverlap_vertical(cageh_real,overlap,sum_vertical)
   END DO

      CALL calculate_crisscross_arithmetic(overlap,sum_horizontal, &
                                sum_vertical)

    END DO
  END DO

!500 CONTINUE

!GO TO 600
!----------------------------------------------------
  overlap = 0
  DO i=1,nhorizontal_4cages
    cage1h = horizontal_4cages(i,1)
    cage2h = horizontal_4cages(i,2)
    cage3h = horizontal_4cages(i,3)
    cage4h = horizontal_4cages(i,4)
    cage1h_real = horizontal_cages(cage1h)
    cage2h_real = horizontal_cages(cage2h)
    cage3h_real = horizontal_cages(cage3h)
    cage4h_real = horizontal_cages(cage4h)
    overlap = 0
    overlap1=0

    ncells = cage_no_of_cells(cage1h_real)
    DO icell=1,ncells
      row1 = cage_to_cells(cage1h_real,icell,1)
      col1 = cage_to_cells(cage1h_real,icell,2)
      overlap(row1,col1,1) = 1
    END DO
    ncells = cage_no_of_cells(cage2h_real)
    DO icell=1,ncells
      row1 = cage_to_cells(cage2h_real,icell,1)
      col1 = cage_to_cells(cage2h_real,icell,2)
      overlap(row1,col1,1) = 1
    END DO
    ncells = cage_no_of_cells(cage3h_real)
    DO icell=1,ncells
      row1 = cage_to_cells(cage3h_real,icell,1)
      col1 = cage_to_cells(cage3h_real,icell,2)
      overlap(row1,col1,1) = 1
    END DO
    ncells = cage_no_of_cells(cage4h_real)
    DO icell=1,ncells
      row1 = cage_to_cells(cage4h_real,icell,1)
      col1 = cage_to_cells(cage4h_real,icell,2)
      overlap(row1,col1,1) = 1
    END DO
    overlap1=overlap

    DO j=1,nvertical_3cages
     cage1v = vertical_3cages(j,1)
     cage2v = vertical_3cages(j,2)
     cage3v = vertical_3cages(j,3)
     cage1v_real = vertical_cages(cage1v)
     cage2v_real = vertical_cages(cage2v)
     cage3v_real = vertical_cages(cage3v)
     overlap=overlap1
     overlap(:,:,2) = 0
     overlap(:,:,3) = 0

     ncells = cage_no_of_cells(cage1v_real)
     DO icell=1,ncells
       row2 = cage_to_cells(cage1v_real,icell,1)
       col2 = cage_to_cells(cage1v_real,icell,2)
       overlap(row2,col2,2) = 1
     END DO
     ncells = cage_no_of_cells(cage2v_real)
     DO icell=1,ncells
       row2 = cage_to_cells(cage2v_real,icell,1)
       col2 = cage_to_cells(cage2v_real,icell,2)
       overlap(row2,col2,2) = 1
     END DO
     ncells = cage_no_of_cells(cage3v_real)
     DO icell=1,ncells
        row2 = cage_to_cells(cage3v_real,icell,1)
        col2 = cage_to_cells(cage3v_real,icell,2)
        overlap(row2,col2,2) = 1
     END DO

      sum_horizontal = cage_sum(cage1h_real) + cage_sum(cage2h_real) + &
			cage_sum(cage3h_real) + cage_sum(cage4h_real)
      sum_vertical = cage_sum(cage1v_real) + cage_sum(cage2v_real) + &
                        cage_sum(cage3v_real)

! Find all the nonoverlapping cells (that are 1 in horizontal and 0 in vertical and
!  vice versa) that have numbers in them.. make the overlap for these cells as 0 and
!  substract the number from the cage_sum..

  CALL number_in_cells_nonoverlap(overlap,sum_horizontal,sum_vertical)

! Similarly find the naked pairs, triples and quads that are in the non_overlapping cells
!  (all the cells should be non overlapping).. make the overlap for these cells as 0 and
!  susbtract the collective sum from the cage_sum
! It is possible that a naked list is duplicated.. so check if the overlap of the cell is 0 or 1..
!   we should remove the duplicate naked list from the cells.. also remove the numbers from
!   naked_values

   ! For horizontal cages
   DO k=1,4
     cageh=horizontal_4cages(i,k)
     cageh_real=horizontal_cages(cageh)
     CALL naked_pairs_nonoverlap_horizontal(cageh_real,overlap,sum_horizontal)
   END DO

   ! For vertical cages
   DO k=1,3
     cageh=vertical_3cages(j,k)
     cageh_real=vertical_cages(cageh)
     CALL naked_pairs_nonoverlap_vertical(cageh_real,overlap,sum_vertical)
   END DO

      CALL calculate_crisscross_arithmetic(overlap,sum_horizontal, &
                                sum_vertical)

    END DO
  END DO

!  600 CONTINUE

!  GO TO 700
!---------------------------------------------------
  overlap = 0
  DO i=1,nhorizontal_4cages
    cage1h = horizontal_4cages(i,1)
    cage2h = horizontal_4cages(i,2)
    cage3h = horizontal_4cages(i,3)
    cage4h = horizontal_4cages(i,4)
    cage1h_real = horizontal_cages(cage1h)
    cage2h_real = horizontal_cages(cage2h)
    cage3h_real = horizontal_cages(cage3h)
    cage4h_real = horizontal_cages(cage4h)
    overlap = 0
    overlap1=0

    ncells = cage_no_of_cells(cage1h_real)
    DO icell=1,ncells
      row1 = cage_to_cells(cage1h_real,icell,1)
      col1 = cage_to_cells(cage1h_real,icell,2)
      overlap(row1,col1,1) = 1
    END DO
    ncells = cage_no_of_cells(cage2h_real)
    DO icell=1,ncells
      row1 = cage_to_cells(cage2h_real,icell,1)
      col1 = cage_to_cells(cage2h_real,icell,2)
      overlap(row1,col1,1) = 1
    END DO
    ncells = cage_no_of_cells(cage3h_real)
    DO icell=1,ncells
      row1 = cage_to_cells(cage3h_real,icell,1)
      col1 = cage_to_cells(cage3h_real,icell,2)
      overlap(row1,col1,1) = 1
    END DO
    ncells = cage_no_of_cells(cage4h_real)
    DO icell=1,ncells
      row1 = cage_to_cells(cage4h_real,icell,1)
      col1 = cage_to_cells(cage4h_real,icell,2)
      overlap(row1,col1,1) = 1
    END DO
    overlap1=overlap

    DO j=1,nvertical_4cages
     cage1v = vertical_4cages(j,1)
     cage2v = vertical_4cages(j,2)
     cage3v = vertical_4cages(j,3)
     cage4v = vertical_4cages(j,4)
     cage1v_real = vertical_cages(cage1v)
     cage2v_real = vertical_cages(cage2v)
     cage3v_real = vertical_cages(cage3v)
     cage4v_real = vertical_cages(cage4v)
     overlap=overlap1
     overlap(:,:,2) = 0
     overlap(:,:,3) = 0

     ncells = cage_no_of_cells(cage1v_real)
     DO icell=1,ncells
       row2 = cage_to_cells(cage1v_real,icell,1)
       col2 = cage_to_cells(cage1v_real,icell,2)
       overlap(row2,col2,2) = 1
     END DO
     ncells = cage_no_of_cells(cage2v_real)
     DO icell=1,ncells
       row2 = cage_to_cells(cage2v_real,icell,1)
       col2 = cage_to_cells(cage2v_real,icell,2)
       overlap(row2,col2,2) = 1
     END DO
     ncells = cage_no_of_cells(cage3v_real)
     DO icell=1,ncells
        row2 = cage_to_cells(cage3v_real,icell,1)
        col2 = cage_to_cells(cage3v_real,icell,2)
        overlap(row2,col2,2) = 1
     END DO
     ncells = cage_no_of_cells(cage4v_real)
     DO icell=1,ncells
        row2 = cage_to_cells(cage4v_real,icell,1)
        col2 = cage_to_cells(cage4v_real,icell,2)
	overlap(row2,col2,2) = 1
     END DO

      sum_horizontal = cage_sum(cage1h_real) + cage_sum(cage2h_real) + &
			cage_sum(cage3h_real) + cage_sum(cage4h_real)
      sum_vertical = cage_sum(cage1v_real) + cage_sum(cage2v_real) + &
                        cage_sum(cage3v_real) + cage_sum(cage4v_real)

! Find all the nonoverlapping cells (that are 1 in horizontal and 0 in vertical and
!  vice versa) that have numbers in them.. make the overlap for these cells as 0 and
!  substract the number from the cage_sum..

  CALL number_in_cells_nonoverlap(overlap,sum_horizontal,sum_vertical)

! Similarly find the naked pairs, triples and quads that are in the non_overlapping cells
!  (all the cells should be non overlapping).. make the overlap for these cells as 0 and
!  susbtract the collective sum from the cage_sum
! It is possible that a naked list is duplicated.. so check if the overlap of the cell is 0 or 1..
!   we should remove the duplicate naked list from the cells.. also remove the numbers from
!   naked_values

   ! For horizontal cages
   DO k=1,4
     cageh=horizontal_4cages(i,k)
     cageh_real=horizontal_cages(cageh)
     CALL naked_pairs_nonoverlap_horizontal(cageh_real,overlap,sum_horizontal)
   END DO

   ! For vertical cages
   DO k=1,4
     cageh=vertical_4cages(j,k)
     cageh_real=vertical_cages(cageh)
     CALL naked_pairs_nonoverlap_vertical(cageh_real,overlap,sum_vertical)
   END DO

      CALL calculate_crisscross_arithmetic(overlap,sum_horizontal, &
                                sum_vertical)

    END DO
  END DO

!700 CONTINUE
!-----------------------------------------------------------
! (3,5) case
  overlap = 0
  DO i=1,nhorizontal_3cages
    overlap = 0
    overlap1=0
    DO k=1,3
      cageh=horizontal_3cages(i,k)
      cageh_real=horizontal_cages(cageh)
      ncells=cage_no_of_cells(cageh_real)
      DO icell=1,ncells
        row1=cage_to_cells(cageh_real,icell,1)
        col1=cage_to_cells(cageh_real,icell,2)
        overlap(row1,col1,1) = 1
      END DO
    END DO
    overlap1=overlap
    ! Find the sum of horizontal_cages
    sum_horizontal=0
    DO k=1,3
      cageh=horizontal_3cages(i,k)
      cageh_real=horizontal_cages(cageh)
      sum_horizontal=sum_horizontal+cage_sum(cageh_real)
    END DO
    sum_horizontal1=sum_horizontal
    DO j=1,nvertical_5cages
      overlap=overlap1
      overlap(:,:,2) = 0
      overlap(:,:,3) = 0
      DO k=1,5
        cagev=vertical_5cages(j,k)
        cagev_real=vertical_cages(cagev)
        ncells=cage_no_of_cells(cagev_real)
        DO icell=1,ncells
          row2=cage_to_cells(cagev_real,icell,1)
          col2=cage_to_cells(cagev_real,icell,2)
          overlap(row2,col2,2) = 1
        END DO
      END DO
      !Find the sum of vertical cages
      sum_vertical=0
      DO k=1,5
        cagev=vertical_5cages(j,k)
	cagev_real=vertical_cages(cagev)
	sum_vertical=sum_vertical+cage_sum(cagev_real)
      END DO

! Find all the nonoverlapping cells (that are 1 in horizontal and 0 in vertical and
!  vice versa) that have numbers in them.. make the overlap for these cells as 0 and
!  substract the number from the cage_sum..

  sum_horizontal=sum_horizontal1

  CALL number_in_cells_nonoverlap(overlap,sum_horizontal,sum_vertical)

! Similarly find the naked pairs, triples and quads that are in the non_overlapping cells
!  (all the cells should be non overlapping).. make the overlap for these cells as 0 and
!  susbtract the collective sum from the cage_sum
! It is possible that a naked list is duplicated.. so check if the overlap of the cell is 0 or 1..
!   we should remove the duplicate naked list from the cells.. also remove the numbers from
!   naked_values

   ! For horizontal cages
   DO k=1,3
     cageh=horizontal_3cages(i,k)
     cageh_real=horizontal_cages(cageh)
     CALL naked_pairs_nonoverlap_horizontal(cageh_real,overlap,sum_horizontal)
   END DO

   ! For vertical cages
   DO k=1,5
     cageh=vertical_5cages(j,k)
     cageh_real=vertical_cages(cageh)
     CALL naked_pairs_nonoverlap_vertical(cageh_real,overlap,sum_vertical)
   END DO

!      CALL calculate_crisscross_arithmetic(overlap,sum_horizontal,sum_vertical,check_arithmetic)
      CALL calculate_crisscross_arithmetic(overlap,sum_horizontal,sum_vertical)
    END DO
  END DO

!-----------------------------------------------------------
! (4,5) case
  overlap = 0
  DO i=1,nhorizontal_4cages
    overlap = 0
    overlap1=0
    DO k=1,4
      cageh=horizontal_4cages(i,k)
      cageh_real=horizontal_cages(cageh)
      ncells=cage_no_of_cells(cageh_real)
      DO icell=1,ncells
        row1=cage_to_cells(cageh_real,icell,1)
        col1=cage_to_cells(cageh_real,icell,2)
        overlap(row1,col1,1) = 1
      END DO
    END DO
    overlap1=overlap
    ! Find the sum of horizontal_cages
    sum_horizontal=0
    DO k=1,4
      cageh=horizontal_4cages(i,k)
      cageh_real=horizontal_cages(cageh)
      sum_horizontal=sum_horizontal+cage_sum(cageh_real)
    END DO
    sum_horizontal1=sum_horizontal
    DO j=1,nvertical_5cages
      overlap=overlap1
      overlap(:,:,2) = 0
      overlap(:,:,3) = 0
      DO k=1,5
        cagev=vertical_5cages(j,k)
        cagev_real=vertical_cages(cagev)
        ncells=cage_no_of_cells(cagev_real)
        DO icell=1,ncells
          row2=cage_to_cells(cagev_real,icell,1)
          col2=cage_to_cells(cagev_real,icell,2)
          overlap(row2,col2,2) = 1
        END DO
      END DO
      !Find the sum of vertical cages
      sum_vertical=0
      DO k=1,5
        cagev=vertical_5cages(j,k)
        cagev_real=vertical_cages(cagev)
        sum_vertical=sum_vertical+cage_sum(cagev_real)
      END DO

! Find all the nonoverlapping cells (that are 1 in horizontal and 0 in vertical and
!  vice versa) that have numbers in them.. make the overlap for these cells as 0 and
!  substract the number from the cage_sum..

  sum_horizontal=sum_horizontal1

  CALL number_in_cells_nonoverlap(overlap,sum_horizontal,sum_vertical)

! Similarly find the naked pairs, triples and quads that are in the non_overlapping cells
!  (all the cells should be non overlapping).. make the overlap for these cells as 0 and
!  susbtract the collective sum from the cage_sum
! It is possible that a naked list is duplicated.. so check if the overlap of the cell is 0 or 1..
!   we should remove the duplicate naked list from the cells.. also remove the numbers from
!   naked_values

   ! For horizontal cages
   DO k=1,4
     cageh=horizontal_4cages(i,k)
     cageh_real=horizontal_cages(cageh)
     CALL naked_pairs_nonoverlap_horizontal(cageh_real,overlap,sum_horizontal)
   END DO

   ! For vertical cages
   DO k=1,5
     cageh=vertical_5cages(j,k)
     cageh_real=vertical_cages(cageh)
     CALL naked_pairs_nonoverlap_vertical(cageh_real,overlap,sum_vertical)
   END DO

!      CALL calculate_crisscross_arithmetic(overlap,sum_horizontal,sum_vertical,check_arithmetic)
      CALL calculate_crisscross_arithmetic(overlap,sum_horizontal,sum_vertical)
    END DO
  END DO
     
!-----------------------------------------------------------
! (5,5) case
  overlap = 0
  DO i=1,nhorizontal_5cages
    overlap = 0
    overlap1=0
    DO k=1,5
      cageh=horizontal_5cages(i,k)
      cageh_real=horizontal_cages(cageh)
      ncells=cage_no_of_cells(cageh_real)
      DO icell=1,ncells
        row1=cage_to_cells(cageh_real,icell,1)
        col1=cage_to_cells(cageh_real,icell,2)
        overlap(row1,col1,1) = 1
      END DO
    END DO
    overlap1=overlap
    ! Find the sum of horizontal_cages
    sum_horizontal=0
    DO k=1,5
      cageh=horizontal_5cages(i,k)
      cageh_real=horizontal_cages(cageh)
      sum_horizontal=sum_horizontal+cage_sum(cageh_real)
    END DO
    sum_horizontal1=sum_horizontal
    DO j=1,nvertical_5cages
      overlap=overlap1
      overlap(:,:,2) = 0
      overlap(:,:,3) = 0
      DO k=1,5
        cagev=vertical_5cages(j,k)
        cagev_real=vertical_cages(cagev)
        ncells=cage_no_of_cells(cagev_real)
        DO icell=1,ncells
          row2=cage_to_cells(cagev_real,icell,1)
          col2=cage_to_cells(cagev_real,icell,2)
          overlap(row2,col2,2) = 1
        END DO
      END DO
      !Find the sum of vertical cages
      sum_vertical=0
      DO k=1,5
        cagev=vertical_5cages(j,k)
        cagev_real=vertical_cages(cagev)
        sum_vertical=sum_vertical+cage_sum(cagev_real)
      END DO

! Find all the nonoverlapping cells (that are 1 in horizontal and 0 in vertical and
!  vice versa) that have numbers in them.. make the overlap for these cells as 0 and
!  substract the number from the cage_sum..

  sum_horizontal=sum_horizontal1

  CALL number_in_cells_nonoverlap(overlap,sum_horizontal,sum_vertical)

! Similarly find the naked pairs, triples and quads that are in the non_overlapping cells
!  (all the cells should be non overlapping).. make the overlap for these cells as 0 and
!  susbtract the collective sum from the cage_sum
! It is possible that a naked list is duplicated.. so check if the overlap of the cell is 0 or 1..
!   we should remove the duplicate naked list from the cells.. also remove the numbers from
!   naked_values

   ! For horizontal cages
   DO k=1,5
     cageh=horizontal_5cages(i,k)
     cageh_real=horizontal_cages(cageh)
     CALL naked_pairs_nonoverlap_horizontal(cageh_real,overlap,sum_horizontal)
   END DO

   ! For vertical cages
   DO k=1,5
     cageh=vertical_5cages(j,k)
     cageh_real=vertical_cages(cageh)
     CALL naked_pairs_nonoverlap_vertical(cageh_real,overlap,sum_vertical)
   END DO

!      CALL calculate_crisscross_arithmetic(overlap,sum_horizontal,sum_vertical,check_arithmetic)
      CALL calculate_crisscross_arithmetic(overlap,sum_horizontal,sum_vertical)
    END DO
  END DO
 
!-----------------------------------------------------------
! (5,3) case
  overlap = 0
  DO i=1,nhorizontal_5cages
    overlap = 0
    overlap1=0
    DO k=1,5
      cageh=horizontal_5cages(i,k)
      cageh_real=horizontal_cages(cageh)
      ncells=cage_no_of_cells(cageh_real)
      DO icell=1,ncells
        row1=cage_to_cells(cageh_real,icell,1)
        col1=cage_to_cells(cageh_real,icell,2)
        overlap(row1,col1,1) = 1
      END DO
    END DO
    overlap1=overlap
    ! Find the sum of horizontal_cages
    sum_horizontal=0
    DO k=1,5
      cageh=horizontal_5cages(i,k)
      cageh_real=horizontal_cages(cageh)
      sum_horizontal=sum_horizontal+cage_sum(cageh_real)
    END DO
    sum_horizontal1=sum_horizontal
    DO j=1,nvertical_3cages
      overlap=overlap1
      overlap(:,:,2) = 0
      overlap(:,:,3) = 0
      DO k=1,3
        cagev=vertical_3cages(j,k)
        cagev_real=vertical_cages(cagev)
        ncells=cage_no_of_cells(cagev_real)
        DO icell=1,ncells
          row2=cage_to_cells(cagev_real,icell,1)
          col2=cage_to_cells(cagev_real,icell,2)
          overlap(row2,col2,2) = 1
        END DO
      END DO
      !Find the sum of vertical cages
      sum_vertical=0
      DO k=1,3
        cagev=vertical_3cages(j,k)
        cagev_real=vertical_cages(cagev)
        sum_vertical=sum_vertical+cage_sum(cagev_real)
      END DO

! Find all the nonoverlapping cells (that are 1 in horizontal and 0 in vertical and
!  vice versa) that have numbers in them.. make the overlap for these cells as 0 and
!  substract the number from the cage_sum..

  sum_horizontal=sum_horizontal1

  CALL number_in_cells_nonoverlap(overlap,sum_horizontal,sum_vertical)

! Similarly find the naked pairs, triples and quads that are in the non_overlapping cells
!  (all the cells should be non overlapping).. make the overlap for these cells as 0 and
!  susbtract the collective sum from the cage_sum
! It is possible that a naked list is duplicated.. so check if the overlap of the cell is 0 or 1..
!   we should remove the duplicate naked list from the cells.. also remove the numbers from
!   naked_values

   ! For horizontal cages
   DO k=1,5
     cageh=horizontal_5cages(i,k)
     cageh_real=horizontal_cages(cageh)
     CALL naked_pairs_nonoverlap_horizontal(cageh_real,overlap,sum_horizontal)
   END DO

   ! For vertical cages
   DO k=1,3
     cageh=vertical_3cages(j,k)
     cageh_real=vertical_cages(cageh)
     CALL naked_pairs_nonoverlap_vertical(cageh_real,overlap,sum_vertical)
   END DO

!      CALL calculate_crisscross_arithmetic(overlap,sum_horizontal,sum_vertical,check_arithmetic)
      CALL calculate_crisscross_arithmetic(overlap,sum_horizontal,sum_vertical)
    END DO
  END DO
     
!-----------------------------------------------------------
! (5,4) case
  overlap = 0
  DO i=1,nhorizontal_5cages
    overlap = 0
    overlap1=0
    DO k=1,5
      cageh=horizontal_5cages(i,k)
      cageh_real=horizontal_cages(cageh)
      ncells=cage_no_of_cells(cageh_real)
      DO icell=1,ncells
        row1=cage_to_cells(cageh_real,icell,1)
        col1=cage_to_cells(cageh_real,icell,2)
        overlap(row1,col1,1) = 1
      END DO
    END DO
    overlap1=overlap
    ! Find the sum of horizontal_cages
    sum_horizontal=0
    DO k=1,5
      cageh=horizontal_5cages(i,k)
      cageh_real=horizontal_cages(cageh)
      sum_horizontal=sum_horizontal+cage_sum(cageh_real)
    END DO
    sum_horizontal1=sum_horizontal
    DO j=1,nvertical_4cages
      overlap=overlap1
      overlap(:,:,2) = 0
      overlap(:,:,3) = 0
      DO k=1,4
        cagev=vertical_4cages(j,k)
        cagev_real=vertical_cages(cagev)
        ncells=cage_no_of_cells(cagev_real)
        DO icell=1,ncells
          row2=cage_to_cells(cagev_real,icell,1)
          col2=cage_to_cells(cagev_real,icell,2)
          overlap(row2,col2,2) = 1
        END DO
      END DO
      !Find the sum of vertical cages
      sum_vertical=0
      DO k=1,4
        cagev=vertical_4cages(j,k)
        cagev_real=vertical_cages(cagev)
        sum_vertical=sum_vertical+cage_sum(cagev_real)
      END DO

! Find all the nonoverlapping cells (that are 1 in horizontal and 0 in vertical and
!  vice versa) that have numbers in them.. make the overlap for these cells as 0 and
!  substract the number from the cage_sum..

  sum_horizontal=sum_horizontal1

  CALL number_in_cells_nonoverlap(overlap,sum_horizontal,sum_vertical)

! Similarly find the naked pairs, triples and quads that are in the non_overlapping cells
!  (all the cells should be non overlapping).. make the overlap for these cells as 0 and
!  susbtract the collective sum from the cage_sum
! It is possible that a naked list is duplicated.. so check if the overlap of the cell is 0 or 1..
!   we should remove the duplicate naked list from the cells.. also remove the numbers from
!   naked_values

   ! For horizontal cages
   DO k=1,5
     cageh=horizontal_5cages(i,k)
     cageh_real=horizontal_cages(cageh)
     CALL naked_pairs_nonoverlap_horizontal(cageh_real,overlap,sum_horizontal)
   END DO

   ! For vertical cages
   DO k=1,4
     cageh=vertical_4cages(j,k)
     cageh_real=vertical_cages(cageh)
     CALL naked_pairs_nonoverlap_vertical(cageh_real,overlap,sum_vertical)
   END DO

!      CALL calculate_crisscross_arithmetic(overlap,sum_horizontal,sum_vertical,check_arithmetic)
      CALL calculate_crisscross_arithmetic(overlap,sum_horizontal,sum_vertical)
    END DO
  END DO
 
!-----------------------------------------------------------
! (4,6) case
  overlap = 0
  DO i=1,nhorizontal_4cages
    overlap = 0
    overlap1=0
    DO k=1,4
      cageh=horizontal_4cages(i,k)
      cageh_real=horizontal_cages(cageh)
      ncells=cage_no_of_cells(cageh_real)
      DO icell=1,ncells
        row1=cage_to_cells(cageh_real,icell,1)
        col1=cage_to_cells(cageh_real,icell,2)
        overlap(row1,col1,1) = 1
      END DO
    END DO
    overlap1=overlap
    ! Find the sum of horizontal_cages
    sum_horizontal=0
    DO k=1,4
      cageh=horizontal_4cages(i,k)
      cageh_real=horizontal_cages(cageh)
      sum_horizontal=sum_horizontal+cage_sum(cageh_real)
    END DO
    sum_horizontal1=sum_horizontal
    DO j=1,nvertical_6cages
      overlap=overlap1
      overlap(:,:,2) = 0
      overlap(:,:,3) = 0
      DO k=1,6
        cagev=vertical_6cages(j,k)
        cagev_real=vertical_cages(cagev)
        ncells=cage_no_of_cells(cagev_real)
        DO icell=1,ncells
          row2=cage_to_cells(cagev_real,icell,1)
          col2=cage_to_cells(cagev_real,icell,2)
          overlap(row2,col2,2) = 1
        END DO
      END DO
      !Find the sum of vertical cages
      sum_vertical=0
      DO k=1,6
        cagev=vertical_6cages(j,k)
        cagev_real=vertical_cages(cagev)
        sum_vertical=sum_vertical+cage_sum(cagev_real)
      END DO

! Find all the nonoverlapping cells (that are 1 in horizontal and 0 in vertical and
!  vice versa) that have numbers in them.. make the overlap for these cells as 0 and
!  substract the number from the cage_sum..

  sum_horizontal=sum_horizontal1

  CALL number_in_cells_nonoverlap(overlap,sum_horizontal,sum_vertical)

! Similarly find the naked pairs, triples and quads that are in the non_overlapping cells
!  (all the cells should be non overlapping).. make the overlap for these cells as 0 and
!  susbtract the collective sum from the cage_sum
! It is possible that a naked list is duplicated.. so check if the overlap of the cell is 0 or 1..
!   we should remove the duplicate naked list from the cells.. also remove the numbers from
!   naked_values

   ! For horizontal cages
   DO k=1,4
     cageh=horizontal_4cages(i,k)
     cageh_real=horizontal_cages(cageh)
     CALL naked_pairs_nonoverlap_horizontal(cageh_real,overlap,sum_horizontal)
   END DO

   ! For vertical cages
   DO k=1,6
     cageh=vertical_6cages(j,k)
     cageh_real=vertical_cages(cageh)
     CALL naked_pairs_nonoverlap_vertical(cageh_real,overlap,sum_vertical)
   END DO

!      CALL calculate_crisscross_arithmetic(overlap,sum_horizontal,sum_vertical,check_arithmetic)
      CALL calculate_crisscross_arithmetic(overlap,sum_horizontal,sum_vertical)
    END DO
  END DO

!-----------------------------------------------------------
! (5,6) case
  overlap = 0
  DO i=1,nhorizontal_5cages
    overlap = 0
    overlap1=0
    DO k=1,5
      cageh=horizontal_5cages(i,k)
      cageh_real=horizontal_cages(cageh)
      ncells=cage_no_of_cells(cageh_real)
      DO icell=1,ncells
        row1=cage_to_cells(cageh_real,icell,1)
        col1=cage_to_cells(cageh_real,icell,2)
        overlap(row1,col1,1) = 1
      END DO
    END DO
    overlap1=overlap
    ! Find the sum of horizontal_cages
    sum_horizontal=0
    DO k=1,5
      cageh=horizontal_5cages(i,k)
      cageh_real=horizontal_cages(cageh)
      sum_horizontal=sum_horizontal+cage_sum(cageh_real)
    END DO
    sum_horizontal1=sum_horizontal
    DO j=1,nvertical_6cages
      overlap=overlap1
      overlap(:,:,2) = 0
      overlap(:,:,3) = 0
      DO k=1,6
        cagev=vertical_6cages(j,k)
        cagev_real=vertical_cages(cagev)
        ncells=cage_no_of_cells(cagev_real)
        DO icell=1,ncells
          row2=cage_to_cells(cagev_real,icell,1)
          col2=cage_to_cells(cagev_real,icell,2)
          overlap(row2,col2,2) = 1
        END DO
      END DO
      !Find the sum of vertical cages
      sum_vertical=0
      DO k=1,6
        cagev=vertical_6cages(j,k)
        cagev_real=vertical_cages(cagev)
        sum_vertical=sum_vertical+cage_sum(cagev_real)
      END DO

! Find all the nonoverlapping cells (that are 1 in horizontal and 0 in vertical and
!  vice versa) that have numbers in them.. make the overlap for these cells as 0 and
!  substract the number from the cage_sum..

  sum_horizontal=sum_horizontal1

  CALL number_in_cells_nonoverlap(overlap,sum_horizontal,sum_vertical)

! Similarly find the naked pairs, triples and quads that are in the non_overlapping cells
!  (all the cells should be non overlapping).. make the overlap for these cells as 0 and
!  susbtract the collective sum from the cage_sum
! It is possible that a naked list is duplicated.. so check if the overlap of the cell is 0 or 1..
!   we should remove the duplicate naked list from the cells.. also remove the numbers from
!   naked_values

   ! For horizontal cages
   DO k=1,5
     cageh=horizontal_5cages(i,k)
     cageh_real=horizontal_cages(cageh)
     CALL naked_pairs_nonoverlap_horizontal(cageh_real,overlap,sum_horizontal)
   END DO

   ! For vertical cages
   DO k=1,6
     cageh=vertical_6cages(j,k)
     cageh_real=vertical_cages(cageh)
     CALL naked_pairs_nonoverlap_vertical(cageh_real,overlap,sum_vertical)
   END DO

!      CALL calculate_crisscross_arithmetic(overlap,sum_horizontal,sum_vertical,check_arithmetic)
      CALL calculate_crisscross_arithmetic(overlap,sum_horizontal,sum_vertical)
    END DO
  END DO

!-----------------------------------------------------------
! (6,6) case
  overlap = 0
  DO i=1,nhorizontal_6cages
    overlap = 0
    overlap1=0
    DO k=1,6
      cageh=horizontal_6cages(i,k)
      cageh_real=horizontal_cages(cageh)
      ncells=cage_no_of_cells(cageh_real)
      DO icell=1,ncells
        row1=cage_to_cells(cageh_real,icell,1)
        col1=cage_to_cells(cageh_real,icell,2)
        overlap(row1,col1,1) = 1
      END DO
    END DO
    overlap1=overlap
    ! Find the sum of horizontal_cages
    sum_horizontal=0
    DO k=1,6
      cageh=horizontal_6cages(i,k)
      cageh_real=horizontal_cages(cageh)
      sum_horizontal=sum_horizontal+cage_sum(cageh_real)
    END DO
    sum_horizontal1=sum_horizontal
    DO j=1,nvertical_6cages
      overlap=overlap1
      overlap(:,:,2) = 0
      overlap(:,:,3) = 0
      DO k=1,6
        cagev=vertical_6cages(j,k)
        cagev_real=vertical_cages(cagev)
        ncells=cage_no_of_cells(cagev_real)
        DO icell=1,ncells
          row2=cage_to_cells(cagev_real,icell,1)
          col2=cage_to_cells(cagev_real,icell,2)
          overlap(row2,col2,2) = 1
        END DO
      END DO
      !Find the sum of vertical cages
      sum_vertical=0
      DO k=1,6
        cagev=vertical_6cages(j,k)
        cagev_real=vertical_cages(cagev)
        sum_vertical=sum_vertical+cage_sum(cagev_real)
      END DO

! Find all the nonoverlapping cells (that are 1 in horizontal and 0 in vertical and
!  vice versa) that have numbers in them.. make the overlap for these cells as 0 and
!  substract the number from the cage_sum..

  sum_horizontal=sum_horizontal1

  CALL number_in_cells_nonoverlap(overlap,sum_horizontal,sum_vertical)

! Similarly find the naked pairs, triples and quads that are in the non_overlapping cells
!  (all the cells should be non overlapping).. make the overlap for these cells as 0 and
!  susbtract the collective sum from the cage_sum
! It is possible that a naked list is duplicated.. so check if the overlap of the cell is 0 or 1..
!   we should remove the duplicate naked list from the cells.. also remove the numbers from
!   naked_values

   ! For horizontal cages
   DO k=1,6
     cageh=horizontal_6cages(i,k)
     cageh_real=horizontal_cages(cageh)
     CALL naked_pairs_nonoverlap_horizontal(cageh_real,overlap,sum_horizontal)
   END DO

   ! For vertical cages
   DO k=1,6
     cageh=vertical_6cages(j,k)
     cageh_real=vertical_cages(cageh)
     CALL naked_pairs_nonoverlap_vertical(cageh_real,overlap,sum_vertical)
   END DO

!      CALL calculate_crisscross_arithmetic(overlap,sum_horizontal,sum_vertical,check_arithmetic)
      CALL calculate_crisscross_arithmetic(overlap,sum_horizontal,sum_vertical)
    END DO
  END DO

!-----------------------------------------------------------
! (6,5) case
  overlap = 0
  DO i=1,nhorizontal_6cages
    overlap = 0
    overlap1=0
    DO k=1,6
      cageh=horizontal_6cages(i,k)
      cageh_real=horizontal_cages(cageh)
      ncells=cage_no_of_cells(cageh_real)
      DO icell=1,ncells
        row1=cage_to_cells(cageh_real,icell,1)
        col1=cage_to_cells(cageh_real,icell,2)
        overlap(row1,col1,1) = 1
      END DO
    END DO
    overlap1=overlap
    ! Find the sum of horizontal_cages
    sum_horizontal=0
    DO k=1,6
      cageh=horizontal_6cages(i,k)
      cageh_real=horizontal_cages(cageh)
      sum_horizontal=sum_horizontal+cage_sum(cageh_real)
    END DO
    sum_horizontal1=sum_horizontal
    DO j=1,nvertical_5cages
      overlap=overlap1
      overlap(:,:,2) = 0
      overlap(:,:,3) = 0
      DO k=1,5
        cagev=vertical_5cages(j,k)
        cagev_real=vertical_cages(cagev)
        ncells=cage_no_of_cells(cagev_real)
        DO icell=1,ncells
          row2=cage_to_cells(cagev_real,icell,1)
          col2=cage_to_cells(cagev_real,icell,2)
          overlap(row2,col2,2) = 1
        END DO
      END DO
      !Find the sum of vertical cages
      sum_vertical=0
      DO k=1,5
        cagev=vertical_5cages(j,k)
        cagev_real=vertical_cages(cagev)
        sum_vertical=sum_vertical+cage_sum(cagev_real)
      END DO

! Find all the nonoverlapping cells (that are 1 in horizontal and 0 in vertical and
!  vice versa) that have numbers in them.. make the overlap for these cells as 0 and
!  substract the number from the cage_sum..

  sum_horizontal=sum_horizontal1

  CALL number_in_cells_nonoverlap(overlap,sum_horizontal,sum_vertical)

! Similarly find the naked pairs, triples and quads that are in the non_overlapping cells
!  (all the cells should be non overlapping).. make the overlap for these cells as 0 and
!  susbtract the collective sum from the cage_sum
! It is possible that a naked list is duplicated.. so check if the overlap of the cell is 0 or 1..
!   we should remove the duplicate naked list from the cells.. also remove the numbers from
!   naked_values

   ! For horizontal cages
   DO k=1,6
     cageh=horizontal_6cages(i,k)
     cageh_real=horizontal_cages(cageh)
     CALL naked_pairs_nonoverlap_horizontal(cageh_real,overlap,sum_horizontal)
   END DO

   ! For vertical cages
   DO k=1,5
     cageh=vertical_5cages(j,k)
     cageh_real=vertical_cages(cageh)
     CALL naked_pairs_nonoverlap_vertical(cageh_real,overlap,sum_vertical)
   END DO

!      CALL calculate_crisscross_arithmetic(overlap,sum_horizontal,sum_vertical,check_arithmetic)
      CALL calculate_crisscross_arithmetic(overlap,sum_horizontal,sum_vertical)
    END DO
  END DO

!-----------------------------------------------------------
! (6,4) case
  overlap = 0
  DO i=1,nhorizontal_6cages
    overlap = 0
    overlap1=0
    DO k=1,6
      cageh=horizontal_6cages(i,k)
      cageh_real=horizontal_cages(cageh)
      ncells=cage_no_of_cells(cageh_real)
      DO icell=1,ncells
        row1=cage_to_cells(cageh_real,icell,1)
        col1=cage_to_cells(cageh_real,icell,2)
        overlap(row1,col1,1) = 1
      END DO
    END DO
    overlap1=overlap
    ! Find the sum of horizontal_cages
    sum_horizontal=0
    DO k=1,6
      cageh=horizontal_6cages(i,k)
      cageh_real=horizontal_cages(cageh)
      sum_horizontal=sum_horizontal+cage_sum(cageh_real)
    END DO
    sum_horizontal1=sum_horizontal
    DO j=1,nvertical_4cages
      overlap=overlap1
      overlap(:,:,2) = 0
      overlap(:,:,3) = 0
      DO k=1,4
        cagev=vertical_4cages(j,k)
        cagev_real=vertical_cages(cagev)
        ncells=cage_no_of_cells(cagev_real)
        DO icell=1,ncells
          row2=cage_to_cells(cagev_real,icell,1)
          col2=cage_to_cells(cagev_real,icell,2)
          overlap(row2,col2,2) = 1
        END DO
      END DO
      !Find the sum of vertical cages
      sum_vertical=0
      DO k=1,4
        cagev=vertical_4cages(j,k)
        cagev_real=vertical_cages(cagev)
        sum_vertical=sum_vertical+cage_sum(cagev_real)
      END DO

! Find all the nonoverlapping cells (that are 1 in horizontal and 0 in vertical and
!  vice versa) that have numbers in them.. make the overlap for these cells as 0 and
!  substract the number from the cage_sum..

  sum_horizontal=sum_horizontal1

  CALL number_in_cells_nonoverlap(overlap,sum_horizontal,sum_vertical)

! Similarly find the naked pairs, triples and quads that are in the non_overlapping cells
!  (all the cells should be non overlapping).. make the overlap for these cells as 0 and
!  susbtract the collective sum from the cage_sum
! It is possible that a naked list is duplicated.. so check if the overlap of the cell is 0 or 1..
!   we should remove the duplicate naked list from the cells.. also remove the numbers from
!   naked_values

   ! For horizontal cages
   DO k=1,6
     cageh=horizontal_6cages(i,k)
     cageh_real=horizontal_cages(cageh)
     CALL naked_pairs_nonoverlap_horizontal(cageh_real,overlap,sum_horizontal)
   END DO

   ! For vertical cages
   DO k=1,4
     cageh=vertical_4cages(j,k)
     cageh_real=vertical_cages(cageh)
     CALL naked_pairs_nonoverlap_vertical(cageh_real,overlap,sum_vertical)
   END DO

!      CALL calculate_crisscross_arithmetic(overlap,sum_horizontal,sum_vertical,check_arithmetic)
      CALL calculate_crisscross_arithmetic(overlap,sum_horizontal,sum_vertical)
    END DO
  END DO

!RETURN

!-----------------------------------------------------------
! (5,7) case
  overlap = 0
  DO i=1,nhorizontal_5cages
    overlap = 0
    overlap1=0
    DO k=1,5
      cageh=horizontal_5cages(i,k)
      cageh_real=horizontal_cages(cageh)
      ncells=cage_no_of_cells(cageh_real)
      DO icell=1,ncells
        row1=cage_to_cells(cageh_real,icell,1)
        col1=cage_to_cells(cageh_real,icell,2)
        overlap(row1,col1,1) = 1
      END DO
    END DO
    overlap1=overlap
    ! Find the sum of horizontal_cages
    sum_horizontal=0
    DO k=1,5
      cageh=horizontal_5cages(i,k)
      cageh_real=horizontal_cages(cageh)
      sum_horizontal=sum_horizontal+cage_sum(cageh_real)
    END DO
    sum_horizontal1=sum_horizontal
    DO j=1,nvertical_7cages
      overlap=overlap1
      overlap(:,:,2) = 0
      overlap(:,:,3) = 0
      DO k=1,7
        cagev=vertical_7cages(j,k)
        cagev_real=vertical_cages(cagev)
        ncells=cage_no_of_cells(cagev_real)
        DO icell=1,ncells
          row2=cage_to_cells(cagev_real,icell,1)
          col2=cage_to_cells(cagev_real,icell,2)
          overlap(row2,col2,2) = 1
        END DO
      END DO
      !Find the sum of vertical cages
      sum_vertical=0
      DO k=1,7
        cagev=vertical_7cages(j,k)
        cagev_real=vertical_cages(cagev)
        sum_vertical=sum_vertical+cage_sum(cagev_real)
      END DO

! Find all the nonoverlapping cells (that are 1 in horizontal and 0 in vertical and
!  vice versa) that have numbers in them.. make the overlap for these cells as 0 and
!  substract the number from the cage_sum..

   sum_horizontal=sum_horizontal1

  CALL number_in_cells_nonoverlap(overlap,sum_horizontal,sum_vertical)

! Similarly find the naked pairs, triples and quads that are in the non_overlapping cells
!  (all the cells should be non overlapping).. make the overlap for these cells as 0 and
!  susbtract the collective sum from the cage_sum
! It is possible that a naked list is duplicated.. so check if the overlap of the cell is 0 or 1..
!   we should remove the duplicate naked list from the cells.. also remove the numbers from
!   naked_values

   ! For horizontal cages
   DO k=1,5
     cageh=horizontal_5cages(i,k)
     cageh_real=horizontal_cages(cageh)
     CALL naked_pairs_nonoverlap_horizontal(cageh_real,overlap,sum_horizontal)
   END DO

   ! For vertical cages
   DO k=1,7
     cageh=vertical_7cages(j,k)
     cageh_real=vertical_cages(cageh)
     CALL naked_pairs_nonoverlap_vertical(cageh_real,overlap,sum_vertical)
   END DO

!      CALL calculate_crisscross_arithmetic(overlap,sum_horizontal,sum_vertical,check_arithmetic)
      CALL calculate_crisscross_arithmetic(overlap,sum_horizontal,sum_vertical)
    END DO
  END DO

!-----------------------------------------------------------
! (6,7) case
  overlap = 0
  DO i=1,nhorizontal_6cages
    overlap = 0
    overlap1=0
    DO k=1,6
      cageh=horizontal_6cages(i,k)
      cageh_real=horizontal_cages(cageh)
      ncells=cage_no_of_cells(cageh_real)
      DO icell=1,ncells
        row1=cage_to_cells(cageh_real,icell,1)
        col1=cage_to_cells(cageh_real,icell,2)
        overlap(row1,col1,1) = 1
      END DO
    END DO
    overlap1=overlap
    ! Find the sum of horizontal_cages
    sum_horizontal=0
    DO k=1,6
      cageh=horizontal_6cages(i,k)
      cageh_real=horizontal_cages(cageh)
      sum_horizontal=sum_horizontal+cage_sum(cageh_real)
    END DO
    sum_horizontal1=sum_horizontal
    DO j=1,nvertical_7cages
      overlap=overlap1
      overlap(:,:,2) = 0
      overlap(:,:,3) = 0
      DO k=1,7
        cagev=vertical_7cages(j,k)
        cagev_real=vertical_cages(cagev)
        ncells=cage_no_of_cells(cagev_real)
        DO icell=1,ncells
          row2=cage_to_cells(cagev_real,icell,1)
          col2=cage_to_cells(cagev_real,icell,2)
          overlap(row2,col2,2) = 1
        END DO
      END DO
      !Find the sum of vertical cages
      sum_vertical=0
      DO k=1,7
        cagev=vertical_7cages(j,k)
        cagev_real=vertical_cages(cagev)
        sum_vertical=sum_vertical+cage_sum(cagev_real)
      END DO

! Find all the nonoverlapping cells (that are 1 in horizontal and 0 in vertical and
!  vice versa) that have numbers in them.. make the overlap for these cells as 0 and
!  substract the number from the cage_sum..

  sum_horizontal=sum_horizontal1

  CALL number_in_cells_nonoverlap(overlap,sum_horizontal,sum_vertical)

! Similarly find the naked pairs, triples and quads that are in the non_overlapping cells
!  (all the cells should be non overlapping).. make the overlap for these cells as 0 and
!  susbtract the collective sum from the cage_sum
! It is possible that a naked list is duplicated.. so check if the overlap of the cell is 0 or 1..
!   we should remove the duplicate naked list from the cells.. also remove the numbers from
!   naked_values

   ! For horizontal cages
   DO k=1,6
     cageh=horizontal_6cages(i,k)
     cageh_real=horizontal_cages(cageh)
     CALL naked_pairs_nonoverlap_horizontal(cageh_real,overlap,sum_horizontal)
   END DO

   ! For vertical cages
   DO k=1,7
     cageh=vertical_7cages(j,k)
     cageh_real=vertical_cages(cageh)
     CALL naked_pairs_nonoverlap_vertical(cageh_real,overlap,sum_vertical)
   END DO

!      CALL calculate_crisscross_arithmetic(overlap,sum_horizontal,sum_vertical,check_arithmetic)
      CALL calculate_crisscross_arithmetic(overlap,sum_horizontal,sum_vertical)
    END DO
  END DO

!-----------------------------------------------------------
! (7,7) case
  overlap = 0
  DO i=1,nhorizontal_7cages
    overlap = 0
    overlap1=0
    DO k=1,7
      cageh=horizontal_7cages(i,k)
      cageh_real=horizontal_cages(cageh)
      ncells=cage_no_of_cells(cageh_real)
      DO icell=1,ncells
        row1=cage_to_cells(cageh_real,icell,1)
        col1=cage_to_cells(cageh_real,icell,2)
        overlap(row1,col1,1) = 1
      END DO
    END DO
    overlap1=overlap
    ! Find the sum of horizontal_cages
    sum_horizontal=0
    DO k=1,7
      cageh=horizontal_7cages(i,k)
      cageh_real=horizontal_cages(cageh)
      sum_horizontal=sum_horizontal+cage_sum(cageh_real)
    END DO
    sum_horizontal1=sum_horizontal
    DO j=1,nvertical_7cages
      overlap=overlap1
      overlap(:,:,2) = 0
      overlap(:,:,3) = 0
      DO k=1,7
        cagev=vertical_7cages(j,k)
        cagev_real=vertical_cages(cagev)
        ncells=cage_no_of_cells(cagev_real)
        DO icell=1,ncells
          row2=cage_to_cells(cagev_real,icell,1)
          col2=cage_to_cells(cagev_real,icell,2)
          overlap(row2,col2,2) = 1
        END DO
      END DO
      !Find the sum of vertical cages
      sum_vertical=0
      DO k=1,7
        cagev=vertical_7cages(j,k)
        cagev_real=vertical_cages(cagev)
        sum_vertical=sum_vertical+cage_sum(cagev_real)
      END DO

! Find all the nonoverlapping cells (that are 1 in horizontal and 0 in vertical and
!  vice versa) that have numbers in them.. make the overlap for these cells as 0 and
!  substract the number from the cage_sum..

  sum_horizontal=sum_horizontal1

  CALL number_in_cells_nonoverlap(overlap,sum_horizontal,sum_vertical)

! Similarly find the naked pairs, triples and quads that are in the non_overlapping cells
!  (all the cells should be non overlapping).. make the overlap for these cells as 0 and
!  susbtract the collective sum from the cage_sum
! It is possible that a naked list is duplicated.. so check if the overlap of the cell is 0 or 1..
!   we should remove the duplicate naked list from the cells.. also remove the numbers from
!   naked_values

   ! For horizontal cages
   DO k=1,7
     cageh=horizontal_7cages(i,k)
     cageh_real=horizontal_cages(cageh)
     CALL naked_pairs_nonoverlap_horizontal(cageh_real,overlap,sum_horizontal)
   END DO

   ! For vertical cages
   DO k=1,7
     cageh=vertical_7cages(j,k)
     cageh_real=vertical_cages(cageh)
     CALL naked_pairs_nonoverlap_vertical(cageh_real,overlap,sum_vertical)
   END DO

!      CALL calculate_crisscross_arithmetic(overlap,sum_horizontal,sum_vertical,check_arithmetic)
      CALL calculate_crisscross_arithmetic(overlap,sum_horizontal,sum_vertical)
    END DO
  END DO

!-----------------------------------------------------------
! (7,6) case
  overlap = 0
  DO i=1,nhorizontal_7cages
    overlap = 0
    overlap1=0
    DO k=1,7
      cageh=horizontal_7cages(i,k)
      cageh_real=horizontal_cages(cageh)
      ncells=cage_no_of_cells(cageh_real)
      DO icell=1,ncells
        row1=cage_to_cells(cageh_real,icell,1)
        col1=cage_to_cells(cageh_real,icell,2)
        overlap(row1,col1,1) = 1
      END DO
    END DO
    overlap1=overlap
    ! Find the sum of horizontal_cages
    sum_horizontal=0
    DO k=1,7
      cageh=horizontal_7cages(i,k)
      cageh_real=horizontal_cages(cageh)
      sum_horizontal=sum_horizontal+cage_sum(cageh_real)
    END DO
    sum_horizontal1=sum_horizontal
    DO j=1,nvertical_6cages
      overlap=overlap1
      overlap(:,:,2) = 0
      overlap(:,:,3) = 0
      DO k=1,6
        cagev=vertical_6cages(j,k)
        cagev_real=vertical_cages(cagev)
        ncells=cage_no_of_cells(cagev_real)
        DO icell=1,ncells
          row2=cage_to_cells(cagev_real,icell,1)
          col2=cage_to_cells(cagev_real,icell,2)
          overlap(row2,col2,2) = 1
        END DO
      END DO
      !Find the sum of vertical cages
      sum_vertical=0
      DO k=1,6
        cagev=vertical_6cages(j,k)
        cagev_real=vertical_cages(cagev)
        sum_vertical=sum_vertical+cage_sum(cagev_real)
      END DO

! Find all the nonoverlapping cells (that are 1 in horizontal and 0 in vertical and
!  vice versa) that have numbers in them.. make the overlap for these cells as 0 and
!  substract the number from the cage_sum..

  sum_horizontal=sum_horizontal1

  CALL number_in_cells_nonoverlap(overlap,sum_horizontal,sum_vertical)

! Similarly find the naked pairs, triples and quads that are in the non_overlapping cells
!  (all the cells should be non overlapping).. make the overlap for these cells as 0 and
!  susbtract the collective sum from the cage_sum
! It is possible that a naked list is duplicated.. so check if the overlap of the cell is 0 or 1..
!   we should remove the duplicate naked list from the cells.. also remove the numbers from
!   naked_values

   ! For horizontal cages
   DO k=1,7
     cageh=horizontal_7cages(i,k)
     cageh_real=horizontal_cages(cageh)
     CALL naked_pairs_nonoverlap_horizontal(cageh_real,overlap,sum_horizontal)
   END DO

   ! For vertical cages
   DO k=1,6
     cageh=vertical_6cages(j,k)
     cageh_real=vertical_cages(cageh)
     CALL naked_pairs_nonoverlap_vertical(cageh_real,overlap,sum_vertical)
   END DO

!      CALL calculate_crisscross_arithmetic(overlap,sum_horizontal,sum_vertical,check_arithmetic)
      CALL calculate_crisscross_arithmetic(overlap,sum_horizontal,sum_vertical)
    END DO
  END DO

!-----------------------------------------------------------
! (7,5) case
  overlap = 0
  DO i=1,nhorizontal_7cages
    overlap = 0
    overlap1=0
    DO k=1,7
      cageh=horizontal_7cages(i,k)
      cageh_real=horizontal_cages(cageh)
      ncells=cage_no_of_cells(cageh_real)
      DO icell=1,ncells
        row1=cage_to_cells(cageh_real,icell,1)
        col1=cage_to_cells(cageh_real,icell,2)
        overlap(row1,col1,1) = 1
      END DO
    END DO
    overlap1=overlap
    ! Find the sum of horizontal_cages
    sum_horizontal=0
    DO k=1,7
      cageh=horizontal_7cages(i,k)
      cageh_real=horizontal_cages(cageh)
      sum_horizontal=sum_horizontal+cage_sum(cageh_real)
    END DO
    sum_horizontal1=sum_horizontal
    DO j=1,nvertical_5cages
      overlap=overlap1
      overlap(:,:,2) = 0
      overlap(:,:,3) = 0
      DO k=1,5
        cagev=vertical_5cages(j,k)
        cagev_real=vertical_cages(cagev)
        ncells=cage_no_of_cells(cagev_real)
        DO icell=1,ncells
          row2=cage_to_cells(cagev_real,icell,1)
          col2=cage_to_cells(cagev_real,icell,2)
          overlap(row2,col2,2) = 1
        END DO
      END DO
      !Find the sum of vertical cages
      sum_vertical=0
      DO k=1,5
        cagev=vertical_5cages(j,k)
        cagev_real=vertical_cages(cagev)
        sum_vertical=sum_vertical+cage_sum(cagev_real)
      END DO

! Find all the nonoverlapping cells (that are 1 in horizontal and 0 in vertical and
!  vice versa) that have numbers in them.. make the overlap for these cells as 0 and
!  substract the number from the cage_sum..

  sum_horizontal=sum_horizontal1

  CALL number_in_cells_nonoverlap(overlap,sum_horizontal,sum_vertical)

! Similarly find the naked pairs, triples and quads that are in the non_overlapping cells
!  (all the cells should be non overlapping).. make the overlap for these cells as 0 and
!  susbtract the collective sum from the cage_sum
! It is possible that a naked list is duplicated.. so check if the overlap of the cell is 0 or 1..
!   we should remove the duplicate naked list from the cells.. also remove the numbers from
!   naked_values

   ! For horizontal cages
   DO k=1,7
     cageh=horizontal_7cages(i,k)
     cageh_real=horizontal_cages(cageh)
     CALL naked_pairs_nonoverlap_horizontal(cageh_real,overlap,sum_horizontal)
   END DO

   ! For vertical cages
   DO k=1,5
     cageh=vertical_5cages(j,k)
     cageh_real=vertical_cages(cageh)
     CALL naked_pairs_nonoverlap_vertical(cageh_real,overlap,sum_vertical)
   END DO

!      CALL calculate_crisscross_arithmetic(overlap,sum_horizontal,sum_vertical,check_arithmetic)
      CALL calculate_crisscross_arithmetic(overlap,sum_horizontal,sum_vertical)
    END DO
  END DO

!-----------------------------------------------------------
! (6,8) case
  overlap = 0
  DO i=1,nhorizontal_6cages
    overlap = 0
    overlap1=0
    DO k=1,6
      cageh=horizontal_6cages(i,k)
      cageh_real=horizontal_cages(cageh)
      ncells=cage_no_of_cells(cageh_real)
      DO icell=1,ncells
        row1=cage_to_cells(cageh_real,icell,1)
        col1=cage_to_cells(cageh_real,icell,2)
        overlap(row1,col1,1) = 1
      END DO
    END DO
    overlap1=overlap
    ! Find the sum of horizontal_cages
    sum_horizontal=0
    DO k=1,6
      cageh=horizontal_6cages(i,k)
      cageh_real=horizontal_cages(cageh)
      sum_horizontal=sum_horizontal+cage_sum(cageh_real)
    END DO
    sum_horizontal1=sum_horizontal
    DO j=1,nvertical_8cages
      overlap=overlap1
      overlap(:,:,2) = 0
      overlap(:,:,3) = 0
      DO k=1,8
        cagev=vertical_8cages(j,k)
        cagev_real=vertical_cages(cagev)
        ncells=cage_no_of_cells(cagev_real)
        DO icell=1,ncells
          row2=cage_to_cells(cagev_real,icell,1)
          col2=cage_to_cells(cagev_real,icell,2)
          overlap(row2,col2,2) = 1
        END DO
      END DO
      !Find the sum of vertical cages
      sum_vertical=0
      DO k=1,8
        cagev=vertical_8cages(j,k)
        cagev_real=vertical_cages(cagev)
        sum_vertical=sum_vertical+cage_sum(cagev_real)
      END DO

! Find all the nonoverlapping cells (that are 1 in horizontal and 0 in vertical and
!  vice versa) that have numbers in them.. make the overlap for these cells as 0 and
!  substract the number from the cage_sum..

  sum_horizontal=sum_horizontal1

  CALL number_in_cells_nonoverlap(overlap,sum_horizontal,sum_vertical)

! Similarly find the naked pairs, triples and quads that are in the non_overlapping cells
!  (all the cells should be non overlapping).. make the overlap for these cells as 0 and
!  susbtract the collective sum from the cage_sum
! It is possible that a naked list is duplicated.. so check if the overlap of the cell is 0 or 1..
!   we should remove the duplicate naked list from the cells.. also remove the numbers from
!   naked_values

   ! For horizontal cages
   DO k=1,6
     cageh=horizontal_6cages(i,k)
     cageh_real=horizontal_cages(cageh)
     CALL naked_pairs_nonoverlap_horizontal(cageh_real,overlap,sum_horizontal)
   END DO

   ! For vertical cages
   DO k=1,8
     cageh=vertical_8cages(j,k)
     cageh_real=vertical_cages(cageh)
     CALL naked_pairs_nonoverlap_vertical(cageh_real,overlap,sum_vertical)
   END DO

!      CALL calculate_crisscross_arithmetic(overlap,sum_horizontal,sum_vertical,check_arithmetic)
      CALL calculate_crisscross_arithmetic(overlap,sum_horizontal,sum_vertical)
    END DO
  END DO

!-----------------------------------------------------------
! (7,8) case
  overlap = 0
  DO i=1,nhorizontal_7cages
    overlap = 0
    overlap1=0
    DO k=1,7
      cageh=horizontal_7cages(i,k)
      cageh_real=horizontal_cages(cageh)
      ncells=cage_no_of_cells(cageh_real)
      DO icell=1,ncells
        row1=cage_to_cells(cageh_real,icell,1)
        col1=cage_to_cells(cageh_real,icell,2)
        overlap(row1,col1,1) = 1
      END DO
    END DO
    overlap1=overlap
    ! Find the sum of horizontal_cages
    sum_horizontal=0
    DO k=1,7
      cageh=horizontal_7cages(i,k)
      cageh_real=horizontal_cages(cageh)
      sum_horizontal=sum_horizontal+cage_sum(cageh_real)
    END DO
    sum_horizontal1=sum_horizontal
    DO j=1,nvertical_8cages
      overlap=overlap1
      overlap(:,:,2) = 0
      overlap(:,:,3) = 0
      DO k=1,8
        cagev=vertical_8cages(j,k)
        cagev_real=vertical_cages(cagev)
        ncells=cage_no_of_cells(cagev_real)
        DO icell=1,ncells
          row2=cage_to_cells(cagev_real,icell,1)
          col2=cage_to_cells(cagev_real,icell,2)
          overlap(row2,col2,2) = 1
        END DO
      END DO
      !Find the sum of vertical cages
      sum_vertical=0
      DO k=1,8
        cagev=vertical_8cages(j,k)
        cagev_real=vertical_cages(cagev)
        sum_vertical=sum_vertical+cage_sum(cagev_real)
      END DO

! Find all the nonoverlapping cells (that are 1 in horizontal and 0 in vertical and
!  vice versa) that have numbers in them.. make the overlap for these cells as 0 and
!  substract the number from the cage_sum..

  sum_horizontal=sum_horizontal1

  CALL number_in_cells_nonoverlap(overlap,sum_horizontal,sum_vertical)

! Similarly find the naked pairs, triples and quads that are in the non_overlapping cells
!  (all the cells should be non overlapping).. make the overlap for these cells as 0 and
!  susbtract the collective sum from the cage_sum
! It is possible that a naked list is duplicated.. so check if the overlap of the cell is 0 or 1..
!   we should remove the duplicate naked list from the cells.. also remove the numbers from
!   naked_values

   ! For horizontal cages
   DO k=1,7
     cageh=horizontal_7cages(i,k)
     cageh_real=horizontal_cages(cageh)
     CALL naked_pairs_nonoverlap_horizontal(cageh_real,overlap,sum_horizontal)
   END DO

   ! For vertical cages
   DO k=1,8
     cageh=vertical_8cages(j,k)
     cageh_real=vertical_cages(cageh)
     CALL naked_pairs_nonoverlap_vertical(cageh_real,overlap,sum_vertical)
   END DO

!      CALL calculate_crisscross_arithmetic(overlap,sum_horizontal,sum_vertical,check_arithmetic)
      CALL calculate_crisscross_arithmetic(overlap,sum_horizontal,sum_vertical)
    END DO
  END DO

!-----------------------------------------------------------
! (8,8) case
  overlap = 0
  DO i=1,nhorizontal_8cages
    overlap = 0
    overlap1=0
    DO k=1,8
      cageh=horizontal_8cages(i,k)
      cageh_real=horizontal_cages(cageh)
      ncells=cage_no_of_cells(cageh_real)
      DO icell=1,ncells
        row1=cage_to_cells(cageh_real,icell,1)
        col1=cage_to_cells(cageh_real,icell,2)
        overlap(row1,col1,1) = 1
      END DO
    END DO
    overlap1=overlap
    ! Find the sum of horizontal_cages
    sum_horizontal=0
    DO k=1,8
      cageh=horizontal_8cages(i,k)
      cageh_real=horizontal_cages(cageh)
      sum_horizontal=sum_horizontal+cage_sum(cageh_real)
    END DO
    sum_horizontal1=sum_horizontal
    DO j=1,nvertical_8cages
      overlap=overlap1
      overlap(:,:,2) = 0
      overlap(:,:,3) = 0
      DO k=1,8
        cagev=vertical_8cages(j,k)
        cagev_real=vertical_cages(cagev)
        ncells=cage_no_of_cells(cagev_real)
        DO icell=1,ncells
          row2=cage_to_cells(cagev_real,icell,1)
          col2=cage_to_cells(cagev_real,icell,2)
          overlap(row2,col2,2) = 1
        END DO
      END DO
      !Find the sum of vertical cages
      sum_vertical=0
      DO k=1,8
        cagev=vertical_8cages(j,k)
        cagev_real=vertical_cages(cagev)
        sum_vertical=sum_vertical+cage_sum(cagev_real)
      END DO

! Find all the nonoverlapping cells (that are 1 in horizontal and 0 in vertical and
!  vice versa) that have numbers in them.. make the overlap for these cells as 0 and
!  substract the number from the cage_sum..

  sum_horizontal=sum_horizontal1

  CALL number_in_cells_nonoverlap(overlap,sum_horizontal,sum_vertical)

! Similarly find the naked pairs, triples and quads that are in the non_overlapping cells
!  (all the cells should be non overlapping).. make the overlap for these cells as 0 and
!  susbtract the collective sum from the cage_sum
! It is possible that a naked list is duplicated.. so check if the overlap of the cell is 0 or 1..
!   we should remove the duplicate naked list from the cells.. also remove the numbers from
!   naked_values

   ! For horizontal cages
   DO k=1,8
     cageh=horizontal_8cages(i,k)
     cageh_real=horizontal_cages(cageh)
     CALL naked_pairs_nonoverlap_horizontal(cageh_real,overlap,sum_horizontal)
   END DO

   ! For vertical cages
   DO k=1,8
     cageh=vertical_8cages(j,k)
     cageh_real=vertical_cages(cageh)
     CALL naked_pairs_nonoverlap_vertical(cageh_real,overlap,sum_vertical)
   END DO

!      CALL calculate_crisscross_arithmetic(overlap,sum_horizontal,sum_vertical,check_arithmetic)
      CALL calculate_crisscross_arithmetic(overlap,sum_horizontal,sum_vertical)
    END DO
  END DO

!-----------------------------------------------------------
! (8,7) case
  overlap = 0
  DO i=1,nhorizontal_8cages
    overlap = 0
    overlap1=0
    DO k=1,8
      cageh=horizontal_8cages(i,k)
      cageh_real=horizontal_cages(cageh)
      ncells=cage_no_of_cells(cageh_real)
      DO icell=1,ncells
        row1=cage_to_cells(cageh_real,icell,1)
        col1=cage_to_cells(cageh_real,icell,2)
        overlap(row1,col1,1) = 1
      END DO
    END DO
    overlap1=overlap
    ! Find the sum of horizontal_cages
    sum_horizontal=0
    DO k=1,8
      cageh=horizontal_8cages(i,k)
      cageh_real=horizontal_cages(cageh)
      sum_horizontal=sum_horizontal+cage_sum(cageh_real)
    END DO
    sum_horizontal1=sum_horizontal
    DO j=1,nvertical_7cages
      overlap=overlap1
      overlap(:,:,2) = 0
      overlap(:,:,3) = 0
      DO k=1,7
        cagev=vertical_7cages(j,k)
        cagev_real=vertical_cages(cagev)
        ncells=cage_no_of_cells(cagev_real)
        DO icell=1,ncells
          row2=cage_to_cells(cagev_real,icell,1)
          col2=cage_to_cells(cagev_real,icell,2)
          overlap(row2,col2,2) = 1
        END DO
      END DO
      !Find the sum of vertical cages
      sum_vertical=0
      DO k=1,7
        cagev=vertical_7cages(j,k)
        cagev_real=vertical_cages(cagev)
        sum_vertical=sum_vertical+cage_sum(cagev_real)
      END DO

! Find all the nonoverlapping cells (that are 1 in horizontal and 0 in vertical and
!  vice versa) that have numbers in them.. make the overlap for these cells as 0 and
!  substract the number from the cage_sum..

  sum_horizontal=sum_horizontal1

  CALL number_in_cells_nonoverlap(overlap,sum_horizontal,sum_vertical)

! Similarly find the naked pairs, triples and quads that are in the non_overlapping cells
!  (all the cells should be non overlapping).. make the overlap for these cells as 0 and
!  susbtract the collective sum from the cage_sum
! It is possible that a naked list is duplicated.. so check if the overlap of the cell is 0 or 1..
!   we should remove the duplicate naked list from the cells.. also remove the numbers from
!   naked_values

   ! For horizontal cages
   DO k=1,8
     cageh=horizontal_8cages(i,k)
     cageh_real=horizontal_cages(cageh)
     CALL naked_pairs_nonoverlap_horizontal(cageh_real,overlap,sum_horizontal)
   END DO

   ! For vertical cages
   DO k=1,7
     cageh=vertical_7cages(j,k)
     cageh_real=vertical_cages(cageh)
     CALL naked_pairs_nonoverlap_vertical(cageh_real,overlap,sum_vertical)
   END DO

!      CALL calculate_crisscross_arithmetic(overlap,sum_horizontal,sum_vertical,check_arithmetic)
      CALL calculate_crisscross_arithmetic(overlap,sum_horizontal,sum_vertical)
    END DO
  END DO

!-----------------------------------------------------------
! (8,6) case
  overlap = 0
  DO i=1,nhorizontal_8cages
    overlap = 0
    overlap1=0
    DO k=1,8
      cageh=horizontal_8cages(i,k)
      cageh_real=horizontal_cages(cageh)
      ncells=cage_no_of_cells(cageh_real)
      DO icell=1,ncells
        row1=cage_to_cells(cageh_real,icell,1)
        col1=cage_to_cells(cageh_real,icell,2)
        overlap(row1,col1,1) = 1
      END DO
    END DO
    overlap1=overlap
    ! Find the sum of horizontal_cages
    sum_horizontal=0
    DO k=1,8
      cageh=horizontal_8cages(i,k)
      cageh_real=horizontal_cages(cageh)
      sum_horizontal=sum_horizontal+cage_sum(cageh_real)
    END DO
    sum_horizontal1=sum_horizontal
    DO j=1,nvertical_6cages
      overlap=overlap1
      overlap(:,:,2) = 0
      overlap(:,:,3) = 0
      DO k=1,6
        cagev=vertical_6cages(j,k)
        cagev_real=vertical_cages(cagev)
        ncells=cage_no_of_cells(cagev_real)
        DO icell=1,ncells
          row2=cage_to_cells(cagev_real,icell,1)
          col2=cage_to_cells(cagev_real,icell,2)
          overlap(row2,col2,2) = 1
        END DO
      END DO
      !Find the sum of vertical cages
      sum_vertical=0
      DO k=1,6
        cagev=vertical_6cages(j,k)
        cagev_real=vertical_cages(cagev)
        sum_vertical=sum_vertical+cage_sum(cagev_real)
      END DO

! Find all the nonoverlapping cells (that are 1 in horizontal and 0 in vertical and 
!  vice versa) that have numbers in them.. make the overlap for these cells as 0 and
!  substract the number from the cage_sum..

  sum_horizontal=sum_horizontal1

  CALL number_in_cells_nonoverlap(overlap,sum_horizontal,sum_vertical)

! Similarly find the naked pairs, triples and quads that are in the non_overlapping cells
!  (all the cells should be non overlapping).. make the overlap for these cells as 0 and
!  susbtract the collective sum from the cage_sum
! It is possible that a naked list is duplicated.. so check if the overlap of the cell is 0 or 1..
!   we should remove the duplicate naked list from the cells.. also remove the numbers from
!   naked_values
  
   ! For horizontal cages
   DO k=1,8
     cageh=horizontal_8cages(i,k)
     cageh_real=horizontal_cages(cageh)
     CALL naked_pairs_nonoverlap_horizontal(cageh_real,overlap,sum_horizontal)
   END DO

   ! For vertical cages
   DO k=1,6
     cageh=vertical_6cages(j,k)
     cageh_real=vertical_cages(cageh)
     CALL naked_pairs_nonoverlap_vertical(cageh_real,overlap,sum_vertical)
   END DO

!      CALL calculate_crisscross_arithmetic(overlap,sum_horizontal,sum_vertical,check_arithmetic)
      CALL calculate_crisscross_arithmetic(overlap,sum_horizontal,sum_vertical)
    END DO
  END DO



END SUBROUTINE Crisscross_arithmetic

!-------------------------------------------------------

SUBROUTINE calculate_crisscross_arithmetic(overlap,sum_horizontal, &
                                sum_vertical)

  USE global_variables
  IMPLICIT NONE 

!  INTEGER,DIMENSION(max_2cages,2) :: horizontal_2cages
!  INTEGER,DIMENSION(max_3cages,3) :: horizontal_3cages
!  INTEGER,DIMENSION(max_4cages,4) :: horizontal_4cages
!  INTEGER,DIMENSION(max_2cages,2) :: vertical_2cages
!  INTEGER,DIMENSION(max_3cages,3) :: vertical_3cages
!  INTEGER,DIMENSION(max_4cages,4) :: vertical_4cages
!  INTEGER :: nhorizontal_2cages,nvertical_2cages
!  INTEGER :: nhorizontal_3cages,nvertical_3cages
!  INTEGER :: nhorizontal_4cages,nvertical_4cages
  INTEGER :: i,j,cage1h,cage2h,cage1h_real,cage2h_real
  INTEGER :: cage1v,cage2v,cage1v_real,cage2v_real
  INTEGER,DIMENSION(max_grid_size1,max_grid_size2,3) :: overlap
  INTEGER :: overlapping_cells,nonoverlapping_cells_horizontal
  INTEGER :: nonoverlapping_cells_vertical,nonoverlapping_cells_total
  INTEGER,DIMENSION(non_overlap_cells_horizontal_max,2) :: non_overlap_cells_horizontal
  INTEGER,DIMENSION(non_overlap_cells_vertical_max,2) :: non_overlap_cells_vertical
  INTEGER :: sum_horizontal,sum_vertical,difference
  INTEGER :: row1,col1,row2,col2,cell_numbers1,cell_numbers2
  INTEGER :: cell_numbers3,cell_numbers4,row3,col3,row4,col4
  INTEGER :: npossible_combination
  INTEGER,DIMENSION(9) :: empty_values1,empty_values2
  INTEGER,DIMENSION(9) :: empty_values3,empty_values4
  INTEGER :: cell_logic_number1,cell_logic_number2
  INTEGER :: cell_logic_number3,cell_logic_number4
  INTEGER :: inumber,jnumber,ivalue,icell,ncells,jcell
  INTEGER :: knumber,lnumber
  INTEGER :: ii,jj

     overlapping_cells = 0
     nonoverlapping_cells_horizontal = 0
     nonoverlapping_cells_vertical = 0
     non_overlap_cells_horizontal = 0
     non_overlap_cells_vertical = 0
     DO icell=1,max_grid_size1
       DO jcell=1,max_grid_size1
         IF (overlap(icell,jcell,1) .eq. 1) THEN
	   IF (overlap(icell,jcell,2) .eq. 1) THEN
	     overlapping_cells = overlapping_cells+1
	     overlap(icell,jcell,3) = 1
	   ELSE
	     nonoverlapping_cells_horizontal = nonoverlapping_cells_horizontal + 1
!	     IF (nonoverlapping_cells_horizontal .ge. non_overlap_cells_horizontal_max) THEN
	     IF (nonoverlapping_cells_horizontal .ge. 5) THEN
!	write(*,*) 'nonoverlapping_cells_horizontal .ge. non_overlap_cells_horizontal_max'
!	write(*,*) 'increase the size of non_overlap_cells_horizontal_max'
!	          STOP  
		  RETURN
	     END IF
	     non_overlap_cells_horizontal(nonoverlapping_cells_horizontal,1) = icell
	     non_overlap_cells_horizontal(nonoverlapping_cells_horizontal,2) = jcell
           END IF
         END IF
         IF (overlap(icell,jcell,2) .eq. 1) THEN
	   IF (overlap(icell,jcell,1) .eq. 0) THEN
             nonoverlapping_cells_vertical= nonoverlapping_cells_vertical + 1
!             IF (nonoverlapping_cells_vertical .ge. non_overlap_cells_vertical_max) THEN
             IF (nonoverlapping_cells_vertical .ge. 5) THEN
!        write(*,*) 'nonoverlapping_cells_vertical .ge. non_overlap_cells_vertical_max'
!        write(*,*) 'increase the size of non_overlap_cells_vertical_max'
!                  STOP
		   RETURN
             END IF

             non_overlap_cells_vertical(nonoverlapping_cells_vertical,1) = icell
             non_overlap_cells_vertical(nonoverlapping_cells_vertical,2) = jcell
	   END IF
          END IF
        END DO
       END DO



      IF (overlapping_cells .lt. 1) RETURN
      IF (nonoverlapping_cells_horizontal + nonoverlapping_cells_vertical .gt. 4) RETURN
      IF (nonoverlapping_cells_horizontal + nonoverlapping_cells_vertical .lt. 1) RETURN
      nonoverlapping_cells_total = nonoverlapping_cells_horizontal + &
					nonoverlapping_cells_vertical
      
      IF (sum_horizontal .gt. sum_vertical) THEN
	difference = sum_horizontal - sum_vertical
      ELSE
        difference = sum_vertical - sum_horizontal
      END IF
      IF (difference .eq. 0) RETURN


      IF (nonoverlapping_cells_total .eq. 1) THEN
	IF (sum_horizontal .gt. sum_vertical) THEN
	  IF (nonoverlapping_cells_horizontal .eq.1) THEN
	    IF (sudoku(non_overlap_cells_horizontal(1,1), &
			non_overlap_cells_horizontal(1,2)) .eq. 0) THEN
             sudoku_logic_values(non_overlap_cells_horizontal(1,1), &
	    		non_overlap_cells_horizontal(1,2)) = 1
	     sudoku_logic(non_overlap_cells_horizontal(1,1), &
			non_overlap_cells_horizontal(1,2),1) = difference
	    END IF
	  ELSE
	    write(*,*) 'SOMETHING is wrong-1'
	    STOP
	  END IF
	ELSE 
          IF (nonoverlapping_cells_vertical .eq.1) THEN
	    IF (sudoku(non_overlap_cells_vertical(1,1), &
			non_overlap_cells_vertical(1,2)) .eq. 0) THEN
             sudoku_logic_values(non_overlap_cells_vertical(1,1), &
                        non_overlap_cells_vertical(1,2)) = 1
             sudoku_logic(non_overlap_cells_vertical(1,1), &
                        non_overlap_cells_vertical(1,2),1) = difference
	    END IF
          ELSE
            write(*,*) 'SOMETHING is wrong-2'
            STOP
          END IF
        END IF
      END IF

      IF (nonoverlapping_cells_total .eq. 2) THEN
        IF (nonoverlapping_cells_horizontal .eq. 2) THEN
        row1 = non_overlap_cells_horizontal(1,1)
	col1 = non_overlap_cells_horizontal(1,2)
	row2 = non_overlap_cells_horizontal(2,1)
	col2 = non_overlap_cells_horizontal(2,2)
	cell_numbers1 = sudoku_logic_values(row1,col1)
        cell_numbers2 = sudoku_logic_values(row2,col2)
        
        npossible_combination = 0
	empty_values1 = 0
	empty_values2 = 0

        IF (sudoku(row1,col1) .gt. 0) THEN
	  cell_numbers1 = 1
          sudoku_logic(row1,col1,1) = sudoku(row1,col1)
        END IF
        IF (sudoku(row2,col2) .gt. 0) THEN
	  cell_numbers2 = 1
	  sudoku_logic(row2,col2,1) = sudoku(row2,col2)
        END IF	

        DO inumber=1,cell_numbers1
         cell_logic_number1 = sudoku_logic(row1,col1,inumber)
         DO jnumber=1,cell_numbers2
          cell_logic_number2 = sudoku_logic(row2,col2,jnumber)
           IF ((cell_logic_number1 + cell_logic_number2) .eq. difference) THEN
	     npossible_combination = npossible_combination+1
	     empty_values1(cell_logic_number1) = empty_values1(cell_logic_number1) + 1
             empty_values2(cell_logic_number2) = empty_values2(cell_logic_number2) + 1
           END IF
          END DO
         END DO

        IF (sudoku(row1,col1) .gt. 0) THEN
	  sudoku_logic(row1,col1,1) = 0
	END IF
	IF (sudoku(row2,col2) .gt. 0) THEN
	  sudoku_logic(row2,col2,1) = 0
 	END IF

       IF (npossible_combination .gt. 0) THEN
	IF (sudoku(row1,col1) .eq. 0) THEN 
         DO ivalue=1,9
           IF (empty_values1(ivalue) .eq. npossible_combination) THEN
             sudoku_logic_values(row1,col1) = 1
             sudoku_logic(row1,col1,1) = ivalue
	   END IF
         END DO
 	END IF
	IF (sudoku(row2,col2) .eq. 0) THEN
         DO ivalue=1,9
          IF (empty_values2(ivalue) .eq. npossible_combination) THEN
            sudoku_logic_values(row2,col2) = 1
	    sudoku_logic(row2,col2,1) = ivalue
	  END IF
         END DO
	END IF
       END IF
       
        END IF
        IF (nonoverlapping_cells_vertical .eq. 2) THEN
         row1 = non_overlap_cells_vertical(1,1)
	 col1 = non_overlap_cells_vertical(1,2)
	 row2 = non_overlap_cells_vertical(2,1)
         col2 = non_overlap_cells_vertical(2,2)
         cell_numbers1 = sudoku_logic_values(row1,col1)
	 cell_numbers2 = sudoku_logic_values(row2,col2)

         npossible_combination = 0
         empty_values1 = 0
	 empty_values2 = 0

        IF (sudoku(row1,col1) .gt. 0) THEN
          cell_numbers1 = 1
          sudoku_logic(row1,col1,1) = sudoku(row1,col1)
        END IF
        IF (sudoku(row2,col2) .gt. 0) THEN
          cell_numbers2 = 1
          sudoku_logic(row2,col2,1) = sudoku(row2,col2)
        END IF

         DO inumber=1,cell_numbers1
          cell_logic_number1 = sudoku_logic(row1,col1,inumber)
          DO jnumber = 1,cell_numbers2
           cell_logic_number2 = sudoku_logic(row2,col2,jnumber)
	   IF ((cell_logic_number1 + cell_logic_number2) .eq. difference) THEN
	     npossible_combination = npossible_combination+1
	     empty_values1(cell_logic_number1) = empty_values1(cell_logic_number1) + 1
 	     empty_values2(cell_logic_number2) = empty_values2(cell_logic_number2) + 1
	   END IF 
          END DO
         END DO

        IF (sudoku(row1,col1) .gt. 0) THEN
          sudoku_logic(row1,col1,1) = 0
        END IF
        IF (sudoku(row2,col2) .gt. 0) THEN
          sudoku_logic(row2,col2,1) = 0
        END IF

        IF (npossible_combination .gt. 0) THEN
	IF (sudoku(row1,col1) .eq. 0) THEN
	 DO ivalue=1,9
          IF (empty_values1(ivalue) .eq. npossible_combination) THEN
	    sudoku_logic_values(row1,col1) = 1
	    sudoku_logic(row1,col1,1) = ivalue
	  END IF
         END DO
	END IF
	IF (sudoku(row2,col2) .eq. 0) THEN
	 DO ivalue=1,9
	  IF (empty_values2(ivalue) .eq. npossible_combination) THEN
	    sudoku_logic_values(row2,col2) = 1
	    sudoku_logic(row2,col2,1) = ivalue
	  END IF
	 END DO
	END IF
        END IF

	END IF

	IF ((nonoverlapping_cells_horizontal .eq. 1) .AND. &
		(nonoverlapping_cells_vertical .eq. 1)) THEN
	  row1 = non_overlap_cells_horizontal(1,1) 
	  col1 = non_overlap_cells_horizontal(1,2)
	  row2 = non_overlap_cells_vertical(1,1)
	  col2 = non_overlap_cells_vertical(1,2)
	  cell_numbers1 = sudoku_logic_values(row1,col1)
	  cell_numbers2 = sudoku_logic_values(row2,col2)

	  npossible_combination = 0
	  empty_values1 = 0
	  empty_values2 = 0

        IF (sudoku(row1,col1) .gt. 0) THEN
          cell_numbers1 = 1
          sudoku_logic(row1,col1,1) = sudoku(row1,col1)
        END IF
        IF (sudoku(row2,col2) .gt. 0) THEN
          cell_numbers2 = 1
          sudoku_logic(row2,col2,1) = sudoku(row2,col2)
        END IF

	  IF (sum_horizontal .gt. sum_vertical) THEN
	    DO inumber=1,cell_numbers1
	     cell_logic_number1 = sudoku_logic(row1,col1,inumber)
	     DO jnumber=1,cell_numbers2
	      cell_logic_number2 = sudoku_logic(row2,col2,jnumber)
	       IF ((cell_logic_number1 - cell_logic_number2) .eq. difference) THEN
		npossible_combination = npossible_combination+1
		empty_values1(cell_logic_number1) = empty_values1(cell_logic_number1)+1
		empty_values2(cell_logic_number2) = empty_values2(cell_logic_number2)+1
	       END IF
	      END DO
	     END DO	
	  ELSE
            DO inumber=1,cell_numbers1
             cell_logic_number1 = sudoku_logic(row1,col1,inumber)
             DO jnumber=1,cell_numbers2
              cell_logic_number2 = sudoku_logic(row2,col2,jnumber)
               IF ((cell_logic_number2 - cell_logic_number1) .eq. difference) THEN
                npossible_combination = npossible_combination+1
                empty_values1(cell_logic_number1) = empty_values1(cell_logic_number1)+1
                empty_values2(cell_logic_number2) = empty_values2(cell_logic_number2)+1
               END IF
              END DO
             END DO
	  END IF

        IF (sudoku(row1,col1) .gt. 0) THEN
          sudoku_logic(row1,col1,1) = 0
        END IF
        IF (sudoku(row2,col2) .gt. 0) THEN
          sudoku_logic(row2,col2,1) = 0
        END IF

	IF (npossible_combination .gt. 0) THEN
	IF (sudoku(row1,col1) .eq. 0) THEN
         DO ivalue=1,9
           IF (empty_values1(ivalue) .eq. npossible_combination) THEN
             sudoku_logic_values(row1,col1) = 1
             sudoku_logic(row1,col1,1) = ivalue
           END IF
         END DO
	END IF
	IF (sudoku(row2,col2) .eq. 0) THEN
         DO ivalue=1,9
          IF (empty_values2(ivalue) .eq. npossible_combination) THEN
            sudoku_logic_values(row2,col2) = 1
            sudoku_logic(row2,col2,1) = ivalue
          END IF
         END DO
	END IF
	END IF
	  
	END IF
      END IF

      IF (nonoverlapping_cells_total .eq. 3) THEN
       IF (nonoverlapping_cells_horizontal .eq. 3) THEN
        row1 = non_overlap_cells_horizontal(1,1)
        col1 = non_overlap_cells_horizontal(1,2)
        row2 = non_overlap_cells_horizontal(2,1)
        col2 = non_overlap_cells_horizontal(2,2)
	row3 = non_overlap_cells_horizontal(3,1)
	col3 = non_overlap_cells_horizontal(3,2)
        cell_numbers1 = sudoku_logic_values(row1,col1)
        cell_numbers2 = sudoku_logic_values(row2,col2)
 	cell_numbers3 = sudoku_logic_values(row3,col3)

        npossible_combination = 0
	empty_values1 = 0
	empty_values2 = 0
	empty_values3 = 0

        IF (sudoku(row1,col1) .gt. 0) THEN
          cell_numbers1 = 1
          sudoku_logic(row1,col1,1) = sudoku(row1,col1)
        END IF
        IF (sudoku(row2,col2) .gt. 0) THEN
          cell_numbers2 = 1
          sudoku_logic(row2,col2,1) = sudoku(row2,col2)
        END IF
	IF (sudoku(row3,col3) .gt. 0) THEN
	  cell_numbers3 = 1
	  sudoku_logic(row3,col3,1) = sudoku(row3,col3)
	END IF

	DO inumber=1,cell_numbers1
         cell_logic_number1 = sudoku_logic(row1,col1,inumber)
	 DO jnumber=1,cell_numbers2
          cell_logic_number2 = sudoku_logic(row2,col2,jnumber)
!	  IF (cell_logic_number1 .eq. cell_logic_number2) CYCLE
	  DO knumber=1,cell_numbers3
           cell_logic_number3 = sudoku_logic(row3,col3,knumber)
	   IF ((cell_logic_number1 + cell_logic_number2 + cell_logic_number3) .eq. &
			difference) THEN
	     npossible_combination=npossible_combination+1
	     empty_values1(cell_logic_number1) = empty_values1(cell_logic_number1)+1
	     empty_values2(cell_logic_number2) = empty_values2(cell_logic_number2)+1
	     empty_values3(cell_logic_number3) = empty_values3(cell_logic_number3)+1
	   END IF
          END DO
         END DO
        END DO

        IF (sudoku(row1,col1) .gt. 0) THEN
          sudoku_logic(row1,col1,1) = 0
        END IF
        IF (sudoku(row2,col2) .gt. 0) THEN
          sudoku_logic(row2,col2,1) = 0
        END IF
	IF (sudoku(row3,col3) .gt. 0) THEN
	  sudoku_logic(row3,col3,1) = 0
	END IF
	   
	IF (npossible_combination .gt. 0) THEN 
	IF (sudoku(row1,col1) .eq. 0) THEN
         DO ivalue=1,9
           IF (empty_values1(ivalue) .eq. npossible_combination) THEN
             sudoku_logic_values(row1,col1) = 1
             sudoku_logic(row1,col1,1) = ivalue
           END IF
         END DO
	END IF
	IF (sudoku(row2,col2) .eq. 0) THEN
         DO ivalue=1,9
          IF (empty_values2(ivalue) .eq. npossible_combination) THEN
            sudoku_logic_values(row2,col2) = 1
            sudoku_logic(row2,col2,1) = ivalue
          END IF
         END DO
	END IF
	IF (sudoku(row3,col3) .eq. 0) THEN
         DO ivalue=1,9
          IF (empty_values3(ivalue) .eq. npossible_combination) THEN
            sudoku_logic_values(row3,col3) = 1
	    sudoku_logic(row3,col3,1) = ivalue
	  END IF
         END DO 
	END IF
	END IF

	END IF 

	IF((nonoverlapping_cells_horizontal .eq. 2) .AND. &
		(nonoverlapping_cells_vertical .eq. 1)) THEN
          row1 = non_overlap_cells_horizontal(1,1)
          col1 = non_overlap_cells_horizontal(1,2)
	  row2 = non_overlap_cells_horizontal(2,1)
	  col2 = non_overlap_cells_horizontal(2,2)
          row3 = non_overlap_cells_vertical(1,1)
          col3 = non_overlap_cells_vertical(1,2)
          cell_numbers1 = sudoku_logic_values(row1,col1)
          cell_numbers2 = sudoku_logic_values(row2,col2)
	  cell_numbers3 = sudoku_logic_values(row3,col3)

	  npossible_combination = 0
	  empty_values1 = 0
	  empty_values2 = 0
	  empty_values3 = 0

        IF (sudoku(row1,col1) .gt. 0) THEN
          cell_numbers1 = 1
          sudoku_logic(row1,col1,1) = sudoku(row1,col1)
        END IF
        IF (sudoku(row2,col2) .gt. 0) THEN
          cell_numbers2 = 1
          sudoku_logic(row2,col2,1) = sudoku(row2,col2)
        END IF
        IF (sudoku(row3,col3) .gt. 0) THEN
          cell_numbers3 = 1
          sudoku_logic(row3,col3,1) = sudoku(row3,col3)
        END IF
	  
	  IF (sum_horizontal .gt. sum_vertical) THEN
	   DO inumber=1,cell_numbers1
	    cell_logic_number1 = sudoku_logic(row1,col1,inumber)
	    DO jnumber=1,cell_numbers2
             cell_logic_number2 = sudoku_logic(row2,col2,jnumber)
             DO knumber=1,cell_numbers3
              cell_logic_number3 = sudoku_logic(row3,col3,knumber)
              IF ((cell_logic_number1 + cell_logic_number2 - cell_logic_number3) &
			.eq. difference) THEN
		npossible_combination = npossible_combination+1
		empty_values1(cell_logic_number1) = empty_values1(cell_logic_number1)+1
		empty_values2(cell_logic_number2) = empty_values2(cell_logic_number2)+1
		empty_values3(cell_logic_number3) = empty_values3(cell_logic_number3)+1
	       END IF 
	      END DO
	     END DO
	    END DO
	  ELSE
	    DO inumber=1,cell_numbers1
             cell_logic_number1 = sudoku_logic(row1,col1,inumber)
	     DO jnumber=1,cell_numbers2
              cell_logic_number2 = sudoku_logic(row2,col2,jnumber)
	      DO knumber=1,cell_numbers3
               cell_logic_number3 = sudoku_logic(row3,col3,knumber)
	       IF ((cell_logic_number3 - cell_logic_number1 - cell_logic_number2) &
			.eq. difference) THEN
		 npossible_combination = npossible_combination+1
		 empty_values1(cell_logic_number1) = empty_values1(cell_logic_number1)+1
		 empty_values2(cell_logic_number2) = empty_values2(cell_logic_number2)+1
		 empty_values3(cell_logic_number3) = empty_values3(cell_logic_number3)+1
		END IF
	       END DO
	      END DO
	     END DO
          END IF

        IF (sudoku(row1,col1) .gt. 0) THEN
          sudoku_logic(row1,col1,1) = 0
        END IF
        IF (sudoku(row2,col2) .gt. 0) THEN
          sudoku_logic(row2,col2,1) = 0
        END IF
        IF (sudoku(row3,col3) .gt. 0) THEN
          sudoku_logic(row3,col3,1) = 0
        END IF

	IF (npossible_combination .gt. 0) THEN
	IF (sudoku(row1,col1) .eq. 0) THEN
         DO ivalue=1,9
           IF (empty_values1(ivalue) .eq. npossible_combination) THEN
             sudoku_logic_values(row1,col1) = 1
             sudoku_logic(row1,col1,1) = ivalue
           END IF
         END DO
	END IF
	IF (sudoku(row2,col2) .eq. 0) THEN
         DO ivalue=1,9
          IF (empty_values2(ivalue) .eq. npossible_combination) THEN
            sudoku_logic_values(row2,col2) = 1
            sudoku_logic(row2,col2,1) = ivalue
          END IF
         END DO
	END IF
	IF (sudoku(row3,col3) .eq. 0) THEN
         DO ivalue=1,9
          IF (empty_values3(ivalue) .eq. npossible_combination) THEN
            sudoku_logic_values(row3,col3) = 1
            sudoku_logic(row3,col3,1) = ivalue
          END IF
         END DO
	END IF
	END IF

        END IF
        IF((nonoverlapping_cells_horizontal .eq. 1) .AND. &
                (nonoverlapping_cells_vertical .eq. 2)) THEN
          row1 = non_overlap_cells_horizontal(1,1)
          col1 = non_overlap_cells_horizontal(1,2)
          row2 = non_overlap_cells_vertical(1,1)
          col2 = non_overlap_cells_vertical(1,2)
          row3 = non_overlap_cells_vertical(2,1)
          col3 = non_overlap_cells_vertical(2,2)
          cell_numbers1 = sudoku_logic_values(row1,col1)
          cell_numbers2 = sudoku_logic_values(row2,col2)
          cell_numbers3 = sudoku_logic_values(row3,col3)

	  npossible_combination = 0
	  empty_values1 = 0
	  empty_values2 = 0
	  empty_values3 = 0

        IF (sudoku(row1,col1) .gt. 0) THEN
          cell_numbers1 = 1
          sudoku_logic(row1,col1,1) = sudoku(row1,col1)
        END IF
        IF (sudoku(row2,col2) .gt. 0) THEN
          cell_numbers2 = 1
          sudoku_logic(row2,col2,1) = sudoku(row2,col2)
        END IF
        IF (sudoku(row3,col3) .gt. 0) THEN
          cell_numbers3 = 1
          sudoku_logic(row3,col3,1) = sudoku(row3,col3)
        END IF

	  IF (sum_horizontal .gt. sum_vertical) THEN
	  DO inumber=1,cell_numbers1
	   cell_logic_number1 = sudoku_logic(row1,col1,inumber)
	   DO jnumber=1,cell_numbers2
	    cell_logic_number2 = sudoku_logic(row2,col2,jnumber)
	    DO knumber=1,cell_numbers3
	     cell_logic_number3 = sudoku_logic(row3,col3,knumber)
	     IF ((cell_logic_number1 - cell_logic_number2 - cell_logic_number3) &
			.eq. difference) THEN
	       npossible_combination = npossible_combination+1
	       empty_values1(cell_logic_number1) = empty_values1(cell_logic_number1)+1
	       empty_values2(cell_logic_number2) = empty_values2(cell_logic_number2)+1
	       empty_values3(cell_logic_number3) = empty_values3(cell_logic_number3)+1
	      END IF
             END DO
            END DO
           END DO
	  ELSE
          DO inumber=1,cell_numbers1
           cell_logic_number1 = sudoku_logic(row1,col1,inumber)
           DO jnumber=1,cell_numbers2
            cell_logic_number2 = sudoku_logic(row2,col2,jnumber)
            DO knumber=1,cell_numbers3
             cell_logic_number3 = sudoku_logic(row3,col3,knumber)
             IF ((cell_logic_number2 + cell_logic_number3 - cell_logic_number1) &
                        .eq. difference) THEN
               npossible_combination = npossible_combination+1
               empty_values1(cell_logic_number1) = empty_values1(cell_logic_number1)+1
               empty_values2(cell_logic_number2) = empty_values2(cell_logic_number2)+1
               empty_values3(cell_logic_number3) = empty_values3(cell_logic_number3)+1
              END IF
             END DO
            END DO
           END DO
          END IF

        IF (sudoku(row1,col1) .gt. 0) THEN
          sudoku_logic(row1,col1,1) = 0
        END IF
        IF (sudoku(row2,col2) .gt. 0) THEN
          sudoku_logic(row2,col2,1) = 0
        END IF
        IF (sudoku(row3,col3) .gt. 0) THEN
          sudoku_logic(row3,col3,1) = 0
        END IF

	IF (npossible_combination .gt. 0) THEN
	IF (sudoku(row1,col1) .eq. 0) THEN
         DO ivalue=1,9
           IF (empty_values1(ivalue) .eq. npossible_combination) THEN
             sudoku_logic_values(row1,col1) = 1
             sudoku_logic(row1,col1,1) = ivalue
           END IF
         END DO
	END IF
	IF (sudoku(row2,col2) .eq. 0) THEN
         DO ivalue=1,9
          IF (empty_values2(ivalue) .eq. npossible_combination) THEN
            sudoku_logic_values(row2,col2) = 1
            sudoku_logic(row2,col2,1) = ivalue
          END IF
         END DO
	END IF
	IF (sudoku(row3,col3) .eq. 0) THEN
         DO ivalue=1,9
          IF (empty_values3(ivalue) .eq. npossible_combination) THEN
            sudoku_logic_values(row3,col3) = 1
            sudoku_logic(row3,col3,1) = ivalue
          END IF
         END DO
	END IF
	END IF

        END IF
        IF (nonoverlapping_cells_vertical .eq. 3) THEN
        row1 = non_overlap_cells_vertical(1,1)
        col1 = non_overlap_cells_vertical(1,2)
        row2 = non_overlap_cells_vertical(2,1)
        col2 = non_overlap_cells_vertical(2,2)
        row3 = non_overlap_cells_vertical(3,1)
        col3 = non_overlap_cells_vertical(3,2)
        cell_numbers1 = sudoku_logic_values(row1,col1)
        cell_numbers2 = sudoku_logic_values(row2,col2)
        cell_numbers3 = sudoku_logic_values(row3,col3)

        npossible_combination = 0
        empty_values1 = 0
        empty_values2 = 0
        empty_values3 = 0

        IF (sudoku(row1,col1) .gt. 0) THEN
          cell_numbers1 = 1
          sudoku_logic(row1,col1,1) = sudoku(row1,col1)
        END IF
        IF (sudoku(row2,col2) .gt. 0) THEN
          cell_numbers2 = 1
          sudoku_logic(row2,col2,1) = sudoku(row2,col2)
        END IF
        IF (sudoku(row3,col3) .gt. 0) THEN
          cell_numbers3 = 1
          sudoku_logic(row3,col3,1) = sudoku(row3,col3)
        END IF


        DO inumber=1,cell_numbers1
         cell_logic_number1 = sudoku_logic(row1,col1,inumber)
         DO jnumber=1,cell_numbers2
          cell_logic_number2 = sudoku_logic(row2,col2,jnumber)
!          IF (cell_logic_number1 .eq. cell_logic_number2) CYCLE
          DO knumber=1,cell_numbers3
           cell_logic_number3 = sudoku_logic(row3,col3,knumber)
           IF ((cell_logic_number1 + cell_logic_number2 + cell_logic_number3) .eq. &
                        difference) THEN
             npossible_combination=npossible_combination+1
             empty_values1(cell_logic_number1) = empty_values1(cell_logic_number1)+1
             empty_values2(cell_logic_number2) = empty_values2(cell_logic_number2)+1
             empty_values3(cell_logic_number3) = empty_values3(cell_logic_number3)+1
           END IF
          END DO
         END DO
        END DO

        IF (sudoku(row1,col1) .gt. 0) THEN
          sudoku_logic(row1,col1,1) = 0
        END IF
        IF (sudoku(row2,col2) .gt. 0) THEN
          sudoku_logic(row2,col2,1) = 0
        END IF
        IF (sudoku(row3,col3) .gt. 0) THEN
          sudoku_logic(row3,col3,1) = 0
        END IF

	IF (npossible_combination .gt. 0) THEN
	IF (sudoku(row1,col1) .eq. 0) THEN
         DO ivalue=1,9
           IF (empty_values1(ivalue) .eq. npossible_combination) THEN
             sudoku_logic_values(row1,col1) = 1
             sudoku_logic(row1,col1,1) = ivalue
           END IF
         END DO
	END IF
	IF (sudoku(row2,col2) .eq. 0) THEN
         DO ivalue=1,9
          IF (empty_values2(ivalue) .eq. npossible_combination) THEN
            sudoku_logic_values(row2,col2) = 1
            sudoku_logic(row2,col2,1) = ivalue
          END IF
         END DO
	END IF
	IF (sudoku(row3,col3) .eq. 0) THEN
         DO ivalue=1,9
          IF (empty_values3(ivalue) .eq. npossible_combination) THEN
            sudoku_logic_values(row3,col3) = 1
            sudoku_logic(row3,col3,1) = ivalue
          END IF
         END DO
	END IF
	END IF

        END IF
       END IF

       IF (nonoverlapping_cells_total .eq. 4) THEN
        IF ((nonoverlapping_cells_horizontal .eq. 3) .AND. &
		(nonoverlapping_cells_vertical .eq. 1)) THEN
        row1 = non_overlap_cells_horizontal(1,1)
        col1 = non_overlap_cells_horizontal(1,2)
        row2 = non_overlap_cells_horizontal(2,1)
        col2 = non_overlap_cells_horizontal(2,2)
        row3 = non_overlap_cells_horizontal(3,1)
        col3 = non_overlap_cells_horizontal(3,2)
	row4 = non_overlap_cells_vertical(1,1)
	col4 = non_overlap_cells_vertical(1,2)
        cell_numbers1 = sudoku_logic_values(row1,col1)
        cell_numbers2 = sudoku_logic_values(row2,col2)
        cell_numbers3 = sudoku_logic_values(row3,col3)
	cell_numbers4 = sudoku_logic_values(row4,col4)

	npossible_combination = 0
	empty_values1 = 0
	empty_values2 = 0
	empty_values3 = 0
	empty_values4 = 0

        IF (sudoku(row1,col1) .gt. 0) THEN
          cell_numbers1 = 1
          sudoku_logic(row1,col1,1) = sudoku(row1,col1)
        END IF
        IF (sudoku(row2,col2) .gt. 0) THEN
          cell_numbers2 = 1
          sudoku_logic(row2,col2,1) = sudoku(row2,col2)
        END IF
        IF (sudoku(row3,col3) .gt. 0) THEN
          cell_numbers3 = 1
          sudoku_logic(row3,col3,1) = sudoku(row3,col3)
        END IF
	IF (sudoku(row4,col4) .gt. 0) THEN
	  cell_numbers4 = 1
	  sudoku_logic(row4,col4,1) = sudoku(row4,col4)
	END IF


       IF (sum_horizontal .gt. sum_vertical) THEN	
        DO inumber=1,cell_numbers1
         cell_logic_number1 = sudoku_logic(row1,col1,inumber)
         DO jnumber=1,cell_numbers2
          cell_logic_number2 = sudoku_logic(row2,col2,jnumber)
          DO knumber=1,cell_numbers3
	   cell_logic_number3 = sudoku_logic(row3,col3,knumber)
	   DO lnumber=1,cell_numbers4
            cell_logic_number4 = sudoku_logic(row4,col4,lnumber)
	    IF ((cell_logic_number1 + cell_logic_number2 + cell_logic_number3 - &
			cell_logic_number4) .eq. difference) THEN
	      npossible_combination = npossible_combination+1
	      empty_values1(cell_logic_number1) = empty_values1(cell_logic_number1)+1
	      empty_values2(cell_logic_number2) = empty_values2(cell_logic_number2)+1
	      empty_values3(cell_logic_number3) = empty_values3(cell_logic_number3)+1
	      empty_values4(cell_logic_number4) = empty_values4(cell_logic_number4)+1
	    END IF
	   END DO
          END DO
         END DO
        END DO
       ELSE
        DO inumber=1,cell_numbers1
         cell_logic_number1 = sudoku_logic(row1,col1,inumber)
         DO jnumber=1,cell_numbers2
          cell_logic_number2 = sudoku_logic(row2,col2,jnumber)
          DO knumber=1,cell_numbers3
           cell_logic_number3 = sudoku_logic(row3,col3,knumber)
           DO lnumber=1,cell_numbers4
            cell_logic_number4 = sudoku_logic(row4,col4,lnumber)
            IF ((cell_logic_number4 - cell_logic_number1 - cell_logic_number2 - &
                        cell_logic_number3) .eq. difference) THEN
              npossible_combination = npossible_combination+1
              empty_values1(cell_logic_number1) = empty_values1(cell_logic_number1)+1
              empty_values2(cell_logic_number2) = empty_values2(cell_logic_number2)+1
              empty_values3(cell_logic_number3) = empty_values3(cell_logic_number3)+1
              empty_values4(cell_logic_number4) = empty_values4(cell_logic_number4)+1
            END IF
           END DO
          END DO
         END DO
        END DO
       END IF

        IF (sudoku(row1,col1) .gt. 0) THEN
          sudoku_logic(row1,col1,1) = 0
        END IF
        IF (sudoku(row2,col2) .gt. 0) THEN
          sudoku_logic(row2,col2,1) = 0
        END IF
        IF (sudoku(row3,col3) .gt. 0) THEN
          sudoku_logic(row3,col3,1) = 0
        END IF
	IF (sudoku(row4,col4) .gt. 0) THEN
	  sudoku_logic(row4,col4,1) = 0
	END IF

	IF (npossible_combination .gt. 0) THEN
	IF (sudoku(row1,col1) .eq. 0) THEN
         DO ivalue=1,9
           IF (empty_values1(ivalue) .eq. npossible_combination) THEN
             sudoku_logic_values(row1,col1) = 1
             sudoku_logic(row1,col1,1) = ivalue
           END IF
         END DO
	END IF
	IF (sudoku(row2,col2) .eq. 0) THEN
         DO ivalue=1,9
          IF (empty_values2(ivalue) .eq. npossible_combination) THEN
            sudoku_logic_values(row2,col2) = 1
            sudoku_logic(row2,col2,1) = ivalue
          END IF
         END DO
	END IF
	IF (sudoku(row3,col3) .eq. 0) THEN
         DO ivalue=1,9
          IF (empty_values3(ivalue) .eq. npossible_combination) THEN
            sudoku_logic_values(row3,col3) = 1
            sudoku_logic(row3,col3,1) = ivalue
          END IF
         END DO
	END IF
	IF (sudoku(row4,col4) .eq. 0) THEN
         DO ivalue=1,9
          IF (empty_values4(ivalue) .eq. npossible_combination) THEN
            sudoku_logic_values(row4,col4) = 1
            sudoku_logic(row4,col4,1) = ivalue
          END IF
         END DO
	END IF
	END IF

	END IF
        IF ((nonoverlapping_cells_horizontal .eq. 2) .AND. &
                (nonoverlapping_cells_vertical .eq. 2)) THEN
        row1 = non_overlap_cells_horizontal(1,1)
        col1 = non_overlap_cells_horizontal(1,2)
        row2 = non_overlap_cells_horizontal(2,1)
        col2 = non_overlap_cells_horizontal(2,2)
        row3 = non_overlap_cells_vertical(1,1)
        col3 = non_overlap_cells_vertical(1,2)
        row4 = non_overlap_cells_vertical(2,1)
        col4 = non_overlap_cells_vertical(2,2)
        cell_numbers1 = sudoku_logic_values(row1,col1)
        cell_numbers2 = sudoku_logic_values(row2,col2)
        cell_numbers3 = sudoku_logic_values(row3,col3)
        cell_numbers4 = sudoku_logic_values(row4,col4)

        npossible_combination = 0
        empty_values1 = 0
        empty_values2 = 0
        empty_values3 = 0
	empty_values4 = 0

        IF (sudoku(row1,col1) .gt. 0) THEN
          cell_numbers1 = 1
          sudoku_logic(row1,col1,1) = sudoku(row1,col1)
        END IF
        IF (sudoku(row2,col2) .gt. 0) THEN
          cell_numbers2 = 1
          sudoku_logic(row2,col2,1) = sudoku(row2,col2)
        END IF
        IF (sudoku(row3,col3) .gt. 0) THEN
          cell_numbers3 = 1
          sudoku_logic(row3,col3,1) = sudoku(row3,col3)
        END IF
        IF (sudoku(row4,col4) .gt. 0) THEN
          cell_numbers4 = 1
          sudoku_logic(row4,col4,1) = sudoku(row4,col4)
        END IF

	IF (sum_horizontal .gt. sum_vertical) THEN
         DO inumber=1,cell_numbers1
          cell_logic_number1 = sudoku_logic(row1,col1,inumber)
          DO jnumber=1,cell_numbers2
           cell_logic_number2 = sudoku_logic(row2,col2,jnumber)
           DO knumber=1,cell_numbers3
            cell_logic_number3 = sudoku_logic(row3,col3,knumber)
            DO lnumber=1,cell_numbers4
	     cell_logic_number4 = sudoku_logic(row4,col4,lnumber)
	     IF ((cell_logic_number1+cell_logic_number2-cell_logic_number3 - &
			cell_logic_number4) .eq. difference) THEN
		npossible_combination = npossible_combination+1
		empty_values1(cell_logic_number1) = empty_values1(cell_logic_number1)+1
		empty_values2(cell_logic_number2) = empty_values2(cell_logic_number2)+1
		empty_values3(cell_logic_number3) = empty_values3(cell_logic_number3)+1
		empty_values4(cell_logic_number4) = empty_values4(cell_logic_number4)+1
	     END IF
	    END DO
           END DO
          END DO
         END DO
        ELSE
         DO inumber=1,cell_numbers1
          cell_logic_number1 = sudoku_logic(row1,col1,inumber)
          DO jnumber=1,cell_numbers2
           cell_logic_number2 = sudoku_logic(row2,col2,jnumber)
           DO knumber=1,cell_numbers3
            cell_logic_number3 = sudoku_logic(row3,col3,knumber)
            DO lnumber=1,cell_numbers4
             cell_logic_number4 = sudoku_logic(row4,col4,lnumber)
             IF ((cell_logic_number3+cell_logic_number4-cell_logic_number1 - &
                        cell_logic_number2) .eq. difference) THEN
                npossible_combination = npossible_combination+1
                empty_values1(cell_logic_number1) = empty_values1(cell_logic_number1)+1
                empty_values2(cell_logic_number2) = empty_values2(cell_logic_number2)+1
                empty_values3(cell_logic_number3) = empty_values3(cell_logic_number3)+1
                empty_values4(cell_logic_number4) = empty_values4(cell_logic_number4)+1
             END IF
            END DO
           END DO
          END DO
         END DO
	END IF

        IF (sudoku(row1,col1) .gt. 0) THEN
          sudoku_logic(row1,col1,1) = 0
        END IF
        IF (sudoku(row2,col2) .gt. 0) THEN
          sudoku_logic(row2,col2,1) = 0
        END IF
        IF (sudoku(row3,col3) .gt. 0) THEN
          sudoku_logic(row3,col3,1) = 0
        END IF
        IF (sudoku(row4,col4) .gt. 0) THEN
          sudoku_logic(row4,col4,1) = 0
        END IF

       IF (npossible_combination .gt. 0) THEN
	IF (sudoku(row1,col1) .eq. 0) THEN
        DO ivalue=1,9
           IF (empty_values1(ivalue) .eq. npossible_combination) THEN
             sudoku_logic_values(row1,col1) = 1
             sudoku_logic(row1,col1,1) = ivalue
           END IF
         END DO
	END IF
	IF (sudoku(row2,col2) .eq. 0) THEN
         DO ivalue=1,9
          IF (empty_values2(ivalue) .eq. npossible_combination) THEN
            sudoku_logic_values(row2,col2) = 1
            sudoku_logic(row2,col2,1) = ivalue
          END IF
         END DO
	END IF
	IF (sudoku(row3,col3) .eq. 0) THEN
         DO ivalue=1,9
          IF (empty_values3(ivalue) .eq. npossible_combination) THEN
            sudoku_logic_values(row3,col3) = 1
            sudoku_logic(row3,col3,1) = ivalue
          END IF
         END DO
	END IF
	IF (sudoku(row4,col4) .eq. 0) THEN
         DO ivalue=1,9
          IF (empty_values4(ivalue) .eq. npossible_combination) THEN
            sudoku_logic_values(row4,col4) = 1
            sudoku_logic(row4,col4,1) = ivalue
          END IF
         END DO
	END IF
	END IF

        END IF
       IF ((nonoverlapping_cells_horizontal .eq. 1) .AND. &
                (nonoverlapping_cells_vertical .eq. 3)) THEN
        row1 = non_overlap_cells_horizontal(1,1)
        col1 = non_overlap_cells_horizontal(1,2)
        row2 = non_overlap_cells_vertical(1,1)
        col2 = non_overlap_cells_vertical(1,2)
        row3 = non_overlap_cells_vertical(2,1)
        col3 = non_overlap_cells_vertical(2,2)
        row4 = non_overlap_cells_vertical(3,1)
        col4 = non_overlap_cells_vertical(3,2)
        cell_numbers1 = sudoku_logic_values(row1,col1)
        cell_numbers2 = sudoku_logic_values(row2,col2)
        cell_numbers3 = sudoku_logic_values(row3,col3)
        cell_numbers4 = sudoku_logic_values(row4,col4)

        npossible_combination = 0
        empty_values1 = 0
        empty_values2 = 0
        empty_values3 = 0
        empty_values4 = 0

        IF (sudoku(row1,col1) .gt. 0) THEN
          cell_numbers1 = 1
          sudoku_logic(row1,col1,1) = sudoku(row1,col1)
        END IF
        IF (sudoku(row2,col2) .gt. 0) THEN
          cell_numbers2 = 1
          sudoku_logic(row2,col2,1) = sudoku(row2,col2)
        END IF
        IF (sudoku(row3,col3) .gt. 0) THEN
          cell_numbers3 = 1
          sudoku_logic(row3,col3,1) = sudoku(row3,col3)
        END IF
        IF (sudoku(row4,col4) .gt. 0) THEN
          cell_numbers4 = 1
          sudoku_logic(row4,col4,1) = sudoku(row4,col4)
        END IF

       IF (sum_horizontal .gt. sum_vertical) THEN
        DO inumber=1,cell_numbers1
         cell_logic_number1 = sudoku_logic(row1,col1,inumber)
         DO jnumber=1,cell_numbers2
          cell_logic_number2 = sudoku_logic(row2,col2,jnumber)
          DO knumber=1,cell_numbers3
           cell_logic_number3 = sudoku_logic(row3,col3,knumber)
           DO lnumber=1,cell_numbers4
            cell_logic_number4 = sudoku_logic(row4,col4,lnumber)
            IF ((cell_logic_number1 - cell_logic_number2 - cell_logic_number3 - &
                        cell_logic_number4) .eq. difference) THEN
              npossible_combination = npossible_combination+1
              empty_values1(cell_logic_number1) = empty_values1(cell_logic_number1)+1
              empty_values2(cell_logic_number2) = empty_values2(cell_logic_number2)+1
              empty_values3(cell_logic_number3) = empty_values3(cell_logic_number3)+1
              empty_values4(cell_logic_number4) = empty_values4(cell_logic_number4)+1
            END IF
           END DO
          END DO
         END DO
        END DO
       ELSE
        DO inumber=1,cell_numbers1
         cell_logic_number1 = sudoku_logic(row1,col1,inumber)
         DO jnumber=1,cell_numbers2
          cell_logic_number2 = sudoku_logic(row2,col2,jnumber)
          DO knumber=1,cell_numbers3
           cell_logic_number3 = sudoku_logic(row3,col3,knumber)
           DO lnumber=1,cell_numbers4
            cell_logic_number4 = sudoku_logic(row4,col4,lnumber)
            IF ((cell_logic_number2 + cell_logic_number3 + cell_logic_number4 - &
                        cell_logic_number1) .eq. difference) THEN
              npossible_combination = npossible_combination+1
              empty_values1(cell_logic_number1) = empty_values1(cell_logic_number1)+1
              empty_values2(cell_logic_number2) = empty_values2(cell_logic_number2)+1
              empty_values3(cell_logic_number3) = empty_values3(cell_logic_number3)+1
              empty_values4(cell_logic_number4) = empty_values4(cell_logic_number4)+1
            END IF
           END DO
          END DO
         END DO
        END DO
       END IF

        IF (sudoku(row1,col1) .gt. 0) THEN
          sudoku_logic(row1,col1,1) = 0
        END IF
        IF (sudoku(row2,col2) .gt. 0) THEN
          sudoku_logic(row2,col2,1) = 0
        END IF
        IF (sudoku(row3,col3) .gt. 0) THEN
          sudoku_logic(row3,col3,1) = 0
        END IF
        IF (sudoku(row4,col4) .gt. 0) THEN
          sudoku_logic(row4,col4,1) = 0
        END IF

	IF (npossible_combination .gt. 0) THEN
	IF (sudoku(row1,col1) .eq. 0) THEN
         DO ivalue=1,9
           IF (empty_values1(ivalue) .eq. npossible_combination) THEN
             sudoku_logic_values(row1,col1) = 1
             sudoku_logic(row1,col1,1) = ivalue
           END IF
         END DO
	END IF
	IF (sudoku(row2,col2) .eq. 0) THEN
         DO ivalue=1,9
          IF (empty_values2(ivalue) .eq. npossible_combination) THEN
            sudoku_logic_values(row2,col2) = 1
            sudoku_logic(row2,col2,1) = ivalue
          END IF
         END DO
	END IF
	IF (sudoku(row3,col3) .eq. 0) THEN
         DO ivalue=1,9
          IF (empty_values3(ivalue) .eq. npossible_combination) THEN
            sudoku_logic_values(row3,col3) = 1
            sudoku_logic(row3,col3,1) = ivalue
          END IF
         END DO
	END IF
	IF (sudoku(row4,col4) .eq. 0) THEN
         DO ivalue=1,9
          IF (empty_values4(ivalue) .eq. npossible_combination) THEN
            sudoku_logic_values(row4,col4) = 1
            sudoku_logic(row4,col4,1) = ivalue
          END IF
         END DO
	END IF
	END IF

        END IF

       IF (nonoverlapping_cells_horizontal .eq. 4) THEN
        row1 = non_overlap_cells_horizontal(1,1)
        col1 = non_overlap_cells_horizontal(1,2)
        row2 = non_overlap_cells_horizontal(2,1)
        col2 = non_overlap_cells_horizontal(2,2)
        row3 = non_overlap_cells_horizontal(3,1)
        col3 = non_overlap_cells_horizontal(3,2)
	row4 = non_overlap_cells_horizontal(4,1)
	col4 = non_overlap_cells_horizontal(4,2)
        cell_numbers1 = sudoku_logic_values(row1,col1)
        cell_numbers2 = sudoku_logic_values(row2,col2)
        cell_numbers3 = sudoku_logic_values(row3,col3)
	cell_numbers4 = sudoku_logic_values(row4,col4)

        npossible_combination = 0
        empty_values1 = 0
        empty_values2 = 0
        empty_values3 = 0
	empty_values4 = 0

        IF (sudoku(row1,col1) .gt. 0) THEN
          cell_numbers1 = 1
          sudoku_logic(row1,col1,1) = sudoku(row1,col1)
        END IF
        IF (sudoku(row2,col2) .gt. 0) THEN
          cell_numbers2 = 1
          sudoku_logic(row2,col2,1) = sudoku(row2,col2)
        END IF
        IF (sudoku(row3,col3) .gt. 0) THEN
          cell_numbers3 = 1
          sudoku_logic(row3,col3,1) = sudoku(row3,col3)
        END IF
	IF (sudoku(row4,col4) .gt. 0) THEN
	  cell_numbers4 = 1
          sudoku_logic(row4,col4,1) = sudoku(row4,col4)
	END IF

        DO inumber=1,cell_numbers1
         cell_logic_number1 = sudoku_logic(row1,col1,inumber)
         DO jnumber=1,cell_numbers2
          cell_logic_number2 = sudoku_logic(row2,col2,jnumber)
!          IF (cell_logic_number1 .eq. cell_logic_number2) CYCLE
          DO knumber=1,cell_numbers3
           cell_logic_number3 = sudoku_logic(row3,col3,knumber)
	   DO lnumber=1,cell_numbers4
	    cell_logic_number4 = sudoku_logic(row4,col4,lnumber)
           IF ((cell_logic_number1 + cell_logic_number2 + cell_logic_number3 + &
				cell_logic_number4) .eq. &
                        difference) THEN
             npossible_combination=npossible_combination+1
             empty_values1(cell_logic_number1) = empty_values1(cell_logic_number1)+1
             empty_values2(cell_logic_number2) = empty_values2(cell_logic_number2)+1
             empty_values3(cell_logic_number3) = empty_values3(cell_logic_number3)+1
	     empty_values4(cell_logic_number4) = empty_values4(cell_logic_number4)+1
           END IF
          END DO
         END DO
        END DO
       END DO

        IF (sudoku(row1,col1) .gt. 0) THEN
          sudoku_logic(row1,col1,1) = 0
        END IF
        IF (sudoku(row2,col2) .gt. 0) THEN
          sudoku_logic(row2,col2,1) = 0
        END IF
        IF (sudoku(row3,col3) .gt. 0) THEN
          sudoku_logic(row3,col3,1) = 0
        END IF
        IF (sudoku(row4,col4) .gt. 0) THEN
	  sudoku_logic(row4,col4,1) = 0
	END IF

        IF (npossible_combination .gt. 0) THEN
        IF (sudoku(row1,col1) .eq. 0) THEN
         DO ivalue=1,9
           IF (empty_values1(ivalue) .eq. npossible_combination) THEN
             sudoku_logic_values(row1,col1) = 1
             sudoku_logic(row1,col1,1) = ivalue
           END IF
         END DO
        END IF
        IF (sudoku(row2,col2) .eq. 0) THEN
         DO ivalue=1,9
          IF (empty_values2(ivalue) .eq. npossible_combination) THEN
            sudoku_logic_values(row2,col2) = 1
            sudoku_logic(row2,col2,1) = ivalue
          END IF
         END DO
        END IF
        IF (sudoku(row3,col3) .eq. 0) THEN
         DO ivalue=1,9
          IF (empty_values3(ivalue) .eq. npossible_combination) THEN
            sudoku_logic_values(row3,col3) = 1
            sudoku_logic(row3,col3,1) = ivalue
          END IF
         END DO
        END IF
	IF (sudoku(row4,col4) .eq. 0) THEN
	 DO ivalue=1,9
	  IF (empty_values4(ivalue) .eq. npossible_combination) THEN
	    sudoku_logic_values(row4,col4) = 1
	    sudoku_logic(row4,col4,1) = ivalue
	  END IF
	 END DO
	END IF
        END IF

        END IF

        IF (nonoverlapping_cells_vertical .eq. 4) THEN
        row1 = non_overlap_cells_vertical(1,1)
        col1 = non_overlap_cells_vertical(1,2)
        row2 = non_overlap_cells_vertical(2,1)
        col2 = non_overlap_cells_vertical(2,2)
        row3 = non_overlap_cells_vertical(3,1)
        col3 = non_overlap_cells_vertical(3,2)
	row4 = non_overlap_cells_vertical(4,1)
	col4 = non_overlap_cells_vertical(4,2)
        cell_numbers1 = sudoku_logic_values(row1,col1)
        cell_numbers2 = sudoku_logic_values(row2,col2)
        cell_numbers3 = sudoku_logic_values(row3,col3)
	cell_numbers4 = sudoku_logic_values(row4,col4)

        npossible_combination = 0
        empty_values1 = 0
        empty_values2 = 0
        empty_values3 = 0
	empty_values4 = 0

        IF (sudoku(row1,col1) .gt. 0) THEN
          cell_numbers1 = 1
          sudoku_logic(row1,col1,1) = sudoku(row1,col1)
        END IF
        IF (sudoku(row2,col2) .gt. 0) THEN
          cell_numbers2 = 1
          sudoku_logic(row2,col2,1) = sudoku(row2,col2)
        END IF
        IF (sudoku(row3,col3) .gt. 0) THEN
          cell_numbers3 = 1
          sudoku_logic(row3,col3,1) = sudoku(row3,col3)
        END IF
	IF (sudoku(row4,col4) .gt. 0) THEN
	  cell_numbers4 = 1
	  sudoku_logic(row4,col4,1) = sudoku(row4,col4)
	END IF

        DO inumber=1,cell_numbers1
         cell_logic_number1 = sudoku_logic(row1,col1,inumber)
         DO jnumber=1,cell_numbers2
          cell_logic_number2 = sudoku_logic(row2,col2,jnumber)
!          IF (cell_logic_number1 .eq. cell_logic_number2) CYCLE
          DO knumber=1,cell_numbers3
           cell_logic_number3 = sudoku_logic(row3,col3,knumber)
	   DO lnumber=1,cell_numbers4
	    cell_logic_number4 = sudoku_logic(row4,col4,lnumber)
           IF ((cell_logic_number1 + cell_logic_number2 + cell_logic_number3 + &
			cell_logic_number4) .eq. &
                        difference) THEN
             npossible_combination=npossible_combination+1
             empty_values1(cell_logic_number1) = empty_values1(cell_logic_number1)+1
             empty_values2(cell_logic_number2) = empty_values2(cell_logic_number2)+1
             empty_values3(cell_logic_number3) = empty_values3(cell_logic_number3)+1
	     empty_values4(cell_logic_number4) = empty_values4(cell_logic_number4)+1
           END IF
          END DO
         END DO
        END DO
       END DO

        IF (sudoku(row1,col1) .gt. 0) THEN
          sudoku_logic(row1,col1,1) = 0
        END IF
        IF (sudoku(row2,col2) .gt. 0) THEN
          sudoku_logic(row2,col2,1) = 0
        END IF
        IF (sudoku(row3,col3) .gt. 0) THEN
          sudoku_logic(row3,col3,1) = 0
        END IF
	IF (sudoku(row4,col4) .gt. 0) THEN
	  sudoku_logic(row4,col4,1) = 0
	END IF

        IF (npossible_combination .gt. 0) THEN
        IF (sudoku(row1,col1) .eq. 0) THEN
         DO ivalue=1,9
           IF (empty_values1(ivalue) .eq. npossible_combination) THEN
             sudoku_logic_values(row1,col1) = 1
             sudoku_logic(row1,col1,1) = ivalue
           END IF
         END DO
        END IF
        IF (sudoku(row2,col2) .eq. 0) THEN
         DO ivalue=1,9
          IF (empty_values2(ivalue) .eq. npossible_combination) THEN
            sudoku_logic_values(row2,col2) = 1
            sudoku_logic(row2,col2,1) = ivalue
          END IF
         END DO
        END IF
        IF (sudoku(row3,col3) .eq. 0) THEN
         DO ivalue=1,9
          IF (empty_values3(ivalue) .eq. npossible_combination) THEN
            sudoku_logic_values(row3,col3) = 1
            sudoku_logic(row3,col3,1) = ivalue
          END IF
         END DO
        END IF
	IF (sudoku(row4,col4) .eq. 0) THEN
	 DO ivalue=1,9
	  IF (empty_values4(ivalue) .eq. npossible_combination) THEN
	   sudoku_logic_values(row4,col4) = 1
	   sudoku_logic(row4,col4,1) = ivalue
	  END IF
	 END DO
	END IF
        END IF

        END IF
       END IF


END SUBROUTINE calculate_crisscross_arithmetic

!------------------------------------------

SUBROUTINE Find_2permutation_horizontalcage

  USE global_variables
  IMPLICIT NONE

!  INTEGER :: nhorizontal_2cages
!  INTEGER,DIMENSION(max_2cages,2) :: horizontal_2cages

  INTEGER :: i,j,icell,jcell,icage,jcage
  INTEGER :: row1,col1,row2,col2,irow,icol,jrow,jcol
  INTEGER :: ncells1,ncells2
  INTEGER :: cage_temp1,cage_temp2
  INTEGER :: vertical_cage1,vertical_cage2
  INTEGER :: check_connection

GO TO 101
!-------- OLD CODE -------------------------------------------  
  DO i=1,nhorizontal_cages-1
    DO j=i+1,nhorizontal_cages
     nhorizontal_2cages = nhorizontal_2cages + 1
        IF (nhorizontal_2cages .ge. max_2cages) THEN
          write(*,*) 'nhorizontal_2cages is .ge. max_2cages'
          write(*,*) 'increase the size of max_2cages'
          STOP
        END IF

      horizontal_2cages(nhorizontal_2cages,1) = i
      horizontal_2cages(nhorizontal_2cages,2) = j
    END DO
  END DO
!-------- END OLD CODE------------------------------------
101 CONTINUE

 DO i=1,nhorizontal_cages
  icage=horizontal_cages(i)
  ncells1=cage_no_of_cells(icage)
  row1=cage_to_cells(icage,1,1)
  col1=cage_to_cells(icage,1,2)
  ! Look for the horizontal cage that have where row2=row1+1 only
jloop:  DO j=1,nhorizontal_cages
    jcage=horizontal_cages(j)
    IF (jcage .eq. icage) CYCLE
    ncells2=cage_no_of_cells(jcage)
    row2=cage_to_cells(jcage,1,1)
    col2=cage_to_cells(jcage,1,2)
    IF (row2 .ne. row1+1) CYCLE
    ! check if two consecutive horizontal cages share a vertical cage
    check_connection=0
    CALL check_connectivity(icage,jcage,check_connection)
    IF (check_connection .eq. 1) THEN
        ! If execution comes here then that means we have 2 horizontal cages
        ! connected with each other
        nhorizontal_2cages=nhorizontal_2cages+1
        horizontal_2cages(nhorizontal_2cages,1) = i
        horizontal_2cages(nhorizontal_2cages,2) = j
        CYCLE jloop
    END IF
   END DO jloop
 END DO


END SUBROUTINE Find_2permutation_horizontalcage

!------------------------------------------

SUBROUTINE Find_3permutation_horizontalcage

  USE global_variables
  IMPLICIT NONE

!  INTEGER :: nhorizontal_3cages
!  INTEGER,DIMENSION(max_3cages,3) :: horizontal_3cages
  INTEGER,DIMENSION(3) :: new_3cages
  INTEGER :: i,j,k,jcage
  INTEGER :: cage1h,cage2h,cage1h_real,cage2h_real
  INTEGER :: row1,col1,row2,col2,row3,col3
  INTEGER :: ncells1,ncells2,ncells3
  INTEGER :: cage_temp1,cage_temp2
  INTEGER :: vertical_cage1,vertical_cage2
  INTEGER :: row_min,row_max
  INTEGER :: check_connection,check_combo
  INTEGER :: cageh,cageh_real


GO TO 102
!------------------ OLD CODE ------------------------------- 
  DO i=1,nhorizontal_cages-2
    DO j=i+1,nhorizontal_cages-1
      DO k=j+1,nhorizontal_cages
	nhorizontal_3cages = nhorizontal_3cages + 1
        IF (nhorizontal_3cages .ge. max_3cages) THEN
          write(*,*) 'nhorizontal_3cages is .ge. max_3cages'
          write(*,*) 'increase the size of max_3cages'
          STOP
        END IF

	horizontal_3cages(nhorizontal_3cages,1) = i
	horizontal_3cages(nhorizontal_3cages,2) = j
	horizontal_3cages(nhorizontal_3cages,3) = k
       END DO
      END DO
    END DO
!------------ END OLD CODE ---------------------------------
102 CONTINUE

 DO i=1,nhorizontal_2cages
   cage1h=horizontal_2cages(i,1)
   cage2h=horizontal_2cages(i,2)
   cage1h_real=horizontal_cages(cage1h)
   cage2h_real=horizontal_cages(cage2h)
   row1=cage_to_cells(cage1h_real,1,1)
   col1=cage_to_cells(cage1h_real,1,2)
   row2=cage_to_cells(cage2h_real,1,1)
   col2=cage_to_cells(cage2h_real,1,2)
   ncells1=cage_no_of_cells(cage1h_real)
   ncells2=cage_no_of_cells(cage2h_real)
   IF (row1 .lt. row2) THEN
     row_min = row1
     row_max = row2
   ELSE
     row_min=row2
     row_max=row1
   END IF
jloop:   DO j=1,nhorizontal_cages
      jcage=horizontal_cages(j)
      IF (jcage .eq. cage1h_real) CYCLE
      IF (jcage .eq. cage2h_real) CYCLE
      ncells3=cage_no_of_cells(jcage)
      row3=cage_to_cells(jcage,1,1)
      col3=cage_to_cells(jcage,1,2)
      IF ((row3 .lt. row_min-1) .OR. (row3 .gt. row_max+1)) CYCLE
      ! check if two consecutive horizontal cages share a vertical cage
      DO k=1,2
	cageh=horizontal_2cages(i,k)
	cageh_real=horizontal_cages(cageh)
        check_connection=0
        CALL check_connectivity(cageh_real,jcage,check_connection)
        IF (check_connection .eq. 1) THEN
	  ! We have a 3 cage combination
	  new_3cages(1) = cage1h
	  new_3cages(2) = cage2h
	  new_3cages(3) = j
	  ! check if the combination has already been stored
          check_combo=0
	  CALL check_combination_3cages(new_3cages,check_combo)
	  IF (check_combo .eq. 0) THEN
	    ! update the list of 3 horizontal cages
	    nhorizontal_3cages=nhorizontal_3cages+1
	    horizontal_3cages(nhorizontal_3cages,1)=cage1h
	    horizontal_3cages(nhorizontal_3cages,2)=cage2h
	    horizontal_3cages(nhorizontal_3cages,3)=j
          END IF
	  CYCLE jloop
        END IF
      END DO
     END DO jloop
    END DO


END SUBROUTINE Find_3permutation_horizontalcage

!------------------------------------------

SUBROUTINE Find_4permutation_horizontalcage

  USE global_variables
  IMPLICIT NONE

!  INTEGER :: nhorizontal_4cages
!  INTEGER,DIMENSION(max_4cages,4) :: horizontal_4cages
  INTEGER :: i,j,k,l,jcage
  INTEGER,DIMENSION(4):: new_4cages
  INTEGER,DIMENSION(3) :: rows
  INTEGER :: cage1h,cage2h,cage1h_real,cage2h_real
  INTEGER :: cage3h,cage3h_real
  INTEGER :: row1,col1,row2,col2,row3,col3,row4,col4
  INTEGER :: ncells1,ncells2,ncells3,ncells4
  INTEGER :: cage_temp1,cage_temp2
  INTEGER :: vertical_cage1,vertical_cage2
  INTEGER :: row_min,row_max
  INTEGER :: check_connection,check_combo,iarray
  INTEGER :: cageh,cageh_real


GO TO 103
!---------- OLD CODE------------------------------------------
  DO i=1,nhorizontal_cages-3
   DO j=i+1,nhorizontal_cages-2
    DO k=j+1,nhorizontal_cages-1
     DO l=k+1,nhorizontal_cages
       nhorizontal_4cages = nhorizontal_4cages + 1
        IF (nhorizontal_4cages .ge. max_4cages) THEN
          write(*,*) 'nhorizontal_4cages is .ge. max_4cages'
          write(*,*) 'increase the size of max_4cages'
          STOP
        END IF
       horizontal_4cages(nhorizontal_4cages,1) = i
       horizontal_4cages(nhorizontal_4cages,2) = j
       horizontal_4cages(nhorizontal_4cages,3) = k
       horizontal_4cages(nhorizontal_4cages,4) = l
      END DO
     END DO
    END DO
   END DO
!---------- END OLD CODE ------------------------------------
103 CONTINUE

 DO i=1,nhorizontal_3cages
   cage1h=horizontal_3cages(i,1)
   cage2h=horizontal_3cages(i,2)
   cage3h=horizontal_3cages(i,3)
   cage1h_real=horizontal_cages(cage1h)
   cage2h_real=horizontal_cages(cage2h)
   cage3h_real=horizontal_cages(cage3h)
   row1=cage_to_cells(cage1h_real,1,1)
   col1=cage_to_cells(cage1h_real,1,2)
   row2=cage_to_cells(cage2h_real,1,1)
   col2=cage_to_cells(cage2h_real,1,2)
   row3=cage_to_cells(cage3h_real,1,1)
   col3=cage_to_cells(cage3h_real,1,2)
   ncells1=cage_no_of_cells(cage1h_real)
   ncells2=cage_no_of_cells(cage2h_real)
   ncells3=cage_no_of_cells(cage3h_real)
   rows(1)=row1
   rows(2)=row2
   rows(3)=row3
   row_max=0
   row_min=100
   DO iarray=1,3
     IF (row_min .gt. rows(iarray)) THEN
	row_min=rows(iarray)
     END IF
     IF (row_max .lt. rows(iarray)) THEN
	row_max=rows(iarray)
     END IF
   END DO
jloop: DO j=1,nhorizontal_cages
      jcage=horizontal_cages(j)
      IF (jcage .eq. cage1h_real) CYCLE
      IF (jcage .eq. cage2h_real) CYCLE
      IF (jcage .eq. cage3h_real) CYCLE
      ncells4=cage_no_of_cells(jcage)
      row4=cage_to_cells(jcage,1,1)
      col4=cage_to_cells(jcage,1,2)
      IF ((row4 .lt. row_min-1) .OR. (row4 .gt. row_max+1)) CYCLE
      ! check if two consecutive horizontal cages share a vertical cage
      DO k=1,3
	cageh=horizontal_3cages(i,k)
	cageh_real=horizontal_cages(cageh)
        check_connection=0
        CALL check_connectivity(cageh_real,jcage,check_connection)
        IF (check_connection .eq. 1) THEN
          ! We have a 4 cage combination
          new_4cages(1) = cage1h
          new_4cages(2) = cage2h
          new_4cages(3) = cage3h
	  new_4cages(4) = j
          ! check if the combination has already been stored
          check_combo=0
          CALL check_combination_4cages(new_4cages,check_combo)
          IF (check_combo .eq. 0) THEN
            ! update the list of 3 horizontal cages
            nhorizontal_4cages=nhorizontal_4cages+1
            horizontal_4cages(nhorizontal_4cages,1)=cage1h
            horizontal_4cages(nhorizontal_4cages,2)=cage2h
            horizontal_4cages(nhorizontal_4cages,3)=cage3h
	    horizontal_4cages(nhorizontal_4cages,4)=j
          END IF
          CYCLE jloop
        END IF
      END DO
    END DO jloop
  END DO


END SUBROUTINE Find_4permutation_horizontalcage

!------------------------------------------------------

SUBROUTINE Find_5permutation_horizontalcage

  USE global_variables
  IMPLICIT NONE

  INTEGER :: i,j,k,l,jcage
  INTEGER,DIMENSION(5):: new_5cages
  INTEGER,DIMENSION(4) :: rows
  INTEGER :: cage1h,cage2h,cage1h_real,cage2h_real
  INTEGER :: cage3h,cage3h_real,cage4h,cage4h_real
  INTEGER :: row1,col1,row2,col2,row3,col3,row4,col4,row5,col5
  INTEGER :: ncells1,ncells2,ncells3,ncells4,ncells5
  INTEGER :: cage_temp1,cage_temp2
  INTEGER :: vertical_cage1,vertical_cage2
  INTEGER :: row_min,row_max
  INTEGER :: check_connection,check_combo,iarray
  INTEGER :: cageh,cageh_real

 DO i=1,nhorizontal_4cages
   cage1h=horizontal_4cages(i,1)
   cage2h=horizontal_4cages(i,2)
   cage3h=horizontal_4cages(i,3)
   cage4h=horizontal_4cages(i,4)
   cage1h_real=horizontal_cages(cage1h)
   cage2h_real=horizontal_cages(cage2h)
   cage3h_real=horizontal_cages(cage3h)
   cage4h_real=horizontal_cages(cage4h)
   row1=cage_to_cells(cage1h_real,1,1)
   col1=cage_to_cells(cage1h_real,1,2)
   row2=cage_to_cells(cage2h_real,1,1)
   col2=cage_to_cells(cage2h_real,1,2)
   row3=cage_to_cells(cage3h_real,1,1)
   col3=cage_to_cells(cage3h_real,1,2)
   row4=cage_to_cells(cage4h_real,1,1)
   col4=cage_to_cells(cage4h_real,1,2)
   ncells1=cage_no_of_cells(cage1h_real)
   ncells2=cage_no_of_cells(cage2h_real)
   ncells3=cage_no_of_cells(cage3h_real)
   ncells4=cage_no_of_cells(cage4h_real)
   rows(1)=row1
   rows(2)=row2
   rows(3)=row3
   rows(4)=row4
   row_max=0
   row_min=100
   DO iarray=1,4
     IF (row_min .gt. rows(iarray)) THEN
        row_min=rows(iarray)
     END IF
     IF (row_max .lt. rows(iarray)) THEN
        row_max=rows(iarray)
     END IF
   END DO
jloop: DO j=1,nhorizontal_cages
      jcage=horizontal_cages(j)
      IF (jcage .eq. cage1h_real) CYCLE
      IF (jcage .eq. cage2h_real) CYCLE
      IF (jcage .eq. cage3h_real) CYCLE
      IF (jcage .eq. cage4h_real) CYCLE
      ncells5=cage_no_of_cells(jcage)
      row5=cage_to_cells(jcage,1,1)
      col5=cage_to_cells(jcage,1,2)
      IF ((row5 .lt. row_min-1) .OR. (row5 .gt. row_max+1)) CYCLE
      ! check if two consecutive horizontal cages share a vertical cage
      DO k=1,4
	cageh=horizontal_4cages(i,k)
	cageh_real=horizontal_cages(cageh)
        check_connection=0
        CALL check_connectivity(cageh_real,jcage,check_connection)
        IF (check_connection .eq. 1) THEN
          ! We have a 5 cage combination
          new_5cages(1) = cage1h
          new_5cages(2) = cage2h
          new_5cages(3) = cage3h
          new_5cages(4) = cage4h
          new_5cages(5) = j
          ! check if the combination has already been stored
          check_combo=0
          CALL check_combination_5cages(new_5cages,check_combo)
          IF (check_combo .eq. 0) THEN
            ! update the list of 3 horizontal cages
            nhorizontal_5cages=nhorizontal_5cages+1
            horizontal_5cages(nhorizontal_5cages,1)=cage1h
            horizontal_5cages(nhorizontal_5cages,2)=cage2h
            horizontal_5cages(nhorizontal_5cages,3)=cage3h
            horizontal_5cages(nhorizontal_5cages,4)=cage4h
            horizontal_5cages(nhorizontal_5cages,5)=j
          END IF
          CYCLE jloop
        END IF
      END DO
    END DO jloop
  END DO


END SUBROUTINE Find_5permutation_horizontalcage
!-------------------------------------------------------

SUBROUTINE Find_6permutation_horizontalcage

  USE global_variables
  IMPLICIT NONE

  INTEGER :: i,j,k,l,jcage
  INTEGER,DIMENSION(6):: new_6cages
  INTEGER,DIMENSION(5) :: rows
  INTEGER :: cage1h,cage2h,cage1h_real,cage2h_real
  INTEGER :: cage3h,cage3h_real,cage4h,cage4h_real
  INTEGER :: cage5h,cage5h_real
  INTEGER :: row1,col1,row2,col2,row3,col3,row4,col4,row5,col5
  INTEGER :: row6,col6
  INTEGER :: ncells1,ncells2,ncells3,ncells4,ncells5,ncells6
  INTEGER :: cage_temp1,cage_temp2
  INTEGER :: vertical_cage1,vertical_cage2
  INTEGER :: row_min,row_max
  INTEGER :: check_connection,check_combo,iarray
  INTEGER :: cageh,cageh_real

 DO i=1,nhorizontal_5cages
   cage1h=horizontal_5cages(i,1)
   cage2h=horizontal_5cages(i,2)
   cage3h=horizontal_5cages(i,3)
   cage4h=horizontal_5cages(i,4)
   cage5h=horizontal_5cages(i,5)
   cage1h_real=horizontal_cages(cage1h)
   cage2h_real=horizontal_cages(cage2h)
   cage3h_real=horizontal_cages(cage3h)
   cage4h_real=horizontal_cages(cage4h)
   cage5h_real=horizontal_cages(cage5h)
   row1=cage_to_cells(cage1h_real,1,1)
   col1=cage_to_cells(cage1h_real,1,2)
   row2=cage_to_cells(cage2h_real,1,1)
   col2=cage_to_cells(cage2h_real,1,2)
   row3=cage_to_cells(cage3h_real,1,1)
   col3=cage_to_cells(cage3h_real,1,2)
   row4=cage_to_cells(cage4h_real,1,1)
   col4=cage_to_cells(cage4h_real,1,2)
   row5=cage_to_cells(cage5h_real,1,1)
   col5=cage_to_cells(cage5h_real,1,2)
   ncells1=cage_no_of_cells(cage1h_real)
   ncells2=cage_no_of_cells(cage2h_real)
   ncells3=cage_no_of_cells(cage3h_real)
   ncells4=cage_no_of_cells(cage4h_real)
   ncells5=cage_no_of_cells(cage5h_real)
   rows(1)=row1
   rows(2)=row2
   rows(3)=row3
   rows(4)=row4
   rows(5)=row5
   row_max=0
   row_min=100
   DO iarray=1,5
     IF (row_min .gt. rows(iarray)) THEN
        row_min=rows(iarray)
     END IF
     IF (row_max .lt. rows(iarray)) THEN
        row_max=rows(iarray)
     END IF
   END DO

jloop: DO j=1,nhorizontal_cages
      jcage=horizontal_cages(j)
      IF (jcage .eq. cage1h_real) CYCLE
      IF (jcage .eq. cage2h_real) CYCLE
      IF (jcage .eq. cage3h_real) CYCLE
      IF (jcage .eq. cage4h_real) CYCLE
      IF (jcage .eq. cage5h_real) CYCLE
      ncells6=cage_no_of_cells(jcage)
      row6=cage_to_cells(jcage,1,1)
      col6=cage_to_cells(jcage,1,2)
      IF ((row6 .lt. row_min-1) .OR. (row6 .gt. row_max+1)) CYCLE
      ! check if two consecutive horizontal cages share a vertical cage
      DO k=1,5
        cageh=horizontal_5cages(i,k)
        cageh_real=horizontal_cages(cageh)
        check_connection=0
        CALL check_connectivity(cageh_real,jcage,check_connection)
        IF (check_connection .eq. 1) THEN
          ! We have a 5 cage combination
          new_6cages(1) = cage1h
          new_6cages(2) = cage2h
          new_6cages(3) = cage3h
          new_6cages(4) = cage4h
          new_6cages(5) = cage5h
 	  new_6cages(6) = j
          ! check if the combination has already been stored
          check_combo=0
          CALL check_combination_6cages(new_6cages,check_combo)
          IF (check_combo .eq. 0) THEN
            ! update the list of 3 horizontal cages
            nhorizontal_6cages=nhorizontal_6cages+1
            horizontal_6cages(nhorizontal_6cages,1)=cage1h
            horizontal_6cages(nhorizontal_6cages,2)=cage2h
            horizontal_6cages(nhorizontal_6cages,3)=cage3h
            horizontal_6cages(nhorizontal_6cages,4)=cage4h
            horizontal_6cages(nhorizontal_6cages,5)=cage5h
            horizontal_6cages(nhorizontal_6cages,6)=j
          END IF
          CYCLE jloop
        END IF
      END DO
    END DO jloop
  END DO


END SUBROUTINE Find_6permutation_horizontalcage

!-------------------------------------------------------

SUBROUTINE Find_7permutation_horizontalcage

  USE global_variables
  IMPLICIT NONE

  INTEGER :: i,j,k,l,jcage
  INTEGER,DIMENSION(7):: new_7cages
  INTEGER,DIMENSION(6) :: rows
  INTEGER :: cage1h,cage2h,cage1h_real,cage2h_real
  INTEGER :: cage3h,cage3h_real,cage4h,cage4h_real
  INTEGER :: cage5h,cage5h_real,cage6h,cage6h_real
  INTEGER :: row1,col1,row2,col2,row3,col3,row4,col4,row5,col5
  INTEGER :: row6,col6,row7,col7
  INTEGER :: ncells1,ncells2,ncells3,ncells4,ncells5,ncells6,ncells7
  INTEGER :: cage_temp1,cage_temp2
  INTEGER :: vertical_cage1,vertical_cage2
  INTEGER :: row_min,row_max
  INTEGER :: check_connection,check_combo,iarray
  INTEGER :: cageh,cageh_real

 DO i=1,nhorizontal_6cages
   cage1h=horizontal_6cages(i,1)
   cage2h=horizontal_6cages(i,2)
   cage3h=horizontal_6cages(i,3)
   cage4h=horizontal_6cages(i,4)
   cage5h=horizontal_6cages(i,5)
   cage6h=horizontal_6cages(i,6)
   cage1h_real=horizontal_cages(cage1h)
   cage2h_real=horizontal_cages(cage2h)
   cage3h_real=horizontal_cages(cage3h)
   cage4h_real=horizontal_cages(cage4h)
   cage5h_real=horizontal_cages(cage5h)
   cage6h_real=horizontal_cages(cage6h)
   row1=cage_to_cells(cage1h_real,1,1)
   col1=cage_to_cells(cage1h_real,1,2)
   row2=cage_to_cells(cage2h_real,1,1)
   col2=cage_to_cells(cage2h_real,1,2)
   row3=cage_to_cells(cage3h_real,1,1)
   col3=cage_to_cells(cage3h_real,1,2)
   row4=cage_to_cells(cage4h_real,1,1)
   col4=cage_to_cells(cage4h_real,1,2)
   row5=cage_to_cells(cage5h_real,1,1)
   col5=cage_to_cells(cage5h_real,1,2)
   row6=cage_to_cells(cage6h_real,1,1)
   col6=cage_to_cells(cage6h_real,1,2)
   ncells1=cage_no_of_cells(cage1h_real)
   ncells2=cage_no_of_cells(cage2h_real)
   ncells3=cage_no_of_cells(cage3h_real)
   ncells4=cage_no_of_cells(cage4h_real)
   ncells5=cage_no_of_cells(cage5h_real)
   ncells6=cage_no_of_cells(cage6h_real)
   rows(1)=row1
   rows(2)=row2
   rows(3)=row3
   rows(4)=row4
   rows(5)=row5
   rows(6)=row6
   row_max=0
   row_min=100
   DO iarray=1,6
     IF (row_min .gt. rows(iarray)) THEN
        row_min=rows(iarray)
     END IF
     IF (row_max .lt. rows(iarray)) THEN
        row_max=rows(iarray)
     END IF
   END DO

jloop: DO j=1,nhorizontal_cages
      jcage=horizontal_cages(j)
      IF (jcage .eq. cage1h_real) CYCLE
      IF (jcage .eq. cage2h_real) CYCLE
      IF (jcage .eq. cage3h_real) CYCLE
      IF (jcage .eq. cage4h_real) CYCLE
      IF (jcage .eq. cage5h_real) CYCLE
      IF (jcage .eq. cage6h_real) CYCLE
      ncells7=cage_no_of_cells(jcage)
      row7=cage_to_cells(jcage,1,1)
      col7=cage_to_cells(jcage,1,2)
      IF ((row7 .lt. row_min-1) .OR. (row7 .gt. row_max+1)) CYCLE
      ! check if two consecutive horizontal cages share a vertical cage
      DO k=1,6
        cageh=horizontal_6cages(i,k)
        cageh_real=horizontal_cages(cageh)
        check_connection=0
        CALL check_connectivity(cageh_real,jcage,check_connection)
        IF (check_connection .eq. 1) THEN
          ! We have a 5 cage combination
          new_7cages(1) = cage1h
          new_7cages(2) = cage2h
          new_7cages(3) = cage3h
          new_7cages(4) = cage4h
          new_7cages(5) = cage5h
          new_7cages(6) = cage6h
	  new_7cages(7) = j
          ! check if the combination has already been stored
          check_combo=0
          CALL check_combination_7cages(new_7cages,check_combo)
          IF (check_combo .eq. 0) THEN
            ! update the list of 3 horizontal cages
            nhorizontal_7cages=nhorizontal_7cages+1
            horizontal_7cages(nhorizontal_7cages,1)=cage1h
            horizontal_7cages(nhorizontal_7cages,2)=cage2h
            horizontal_7cages(nhorizontal_7cages,3)=cage3h
            horizontal_7cages(nhorizontal_7cages,4)=cage4h
            horizontal_7cages(nhorizontal_7cages,5)=cage5h
            horizontal_7cages(nhorizontal_7cages,6)=cage6h
            horizontal_7cages(nhorizontal_7cages,7)=j
          END IF
          CYCLE jloop
        END IF
      END DO
    END DO jloop
  END DO


END SUBROUTINE Find_7permutation_horizontalcage

!-------------------------------------------------------

SUBROUTINE Find_8permutation_horizontalcage

  USE global_variables
  IMPLICIT NONE

  INTEGER :: i,j,k,l,jcage
  INTEGER,DIMENSION(8):: new_8cages
  INTEGER,DIMENSION(7) :: rows
  INTEGER :: cage1h,cage2h,cage1h_real,cage2h_real
  INTEGER :: cage3h,cage3h_real,cage4h,cage4h_real
  INTEGER :: cage5h,cage5h_real,cage6h,cage6h_real
  INTEGER :: cage7h,cage7h_real
  INTEGER :: row1,col1,row2,col2,row3,col3,row4,col4,row5,col5
  INTEGER :: row6,col6,row7,col7,row8,col8
  INTEGER :: ncells1,ncells2,ncells3,ncells4,ncells5,ncells6,ncells7,ncells8
  INTEGER :: cage_temp1,cage_temp2
  INTEGER :: vertical_cage1,vertical_cage2
  INTEGER :: row_min,row_max
  INTEGER :: check_connection,check_combo,iarray
  INTEGER :: cageh,cageh_real

DO i=1,nhorizontal_7cages
   cage1h=horizontal_7cages(i,1)
   cage2h=horizontal_7cages(i,2)
   cage3h=horizontal_7cages(i,3)
   cage4h=horizontal_7cages(i,4)
   cage5h=horizontal_7cages(i,5)
   cage6h=horizontal_7cages(i,6)
   cage7h=horizontal_7cages(i,7)
   cage1h_real=horizontal_cages(cage1h)
   cage2h_real=horizontal_cages(cage2h)
   cage3h_real=horizontal_cages(cage3h)
   cage4h_real=horizontal_cages(cage4h)
   cage5h_real=horizontal_cages(cage5h)
   cage6h_real=horizontal_cages(cage6h)
   cage7h_real=horizontal_cages(cage7h)
   row1=cage_to_cells(cage1h_real,1,1)
   col1=cage_to_cells(cage1h_real,1,2)
   row2=cage_to_cells(cage2h_real,1,1)
   col2=cage_to_cells(cage2h_real,1,2)
   row3=cage_to_cells(cage3h_real,1,1)
   col3=cage_to_cells(cage3h_real,1,2)
   row4=cage_to_cells(cage4h_real,1,1)
   col4=cage_to_cells(cage4h_real,1,2)
   row5=cage_to_cells(cage5h_real,1,1)
   col5=cage_to_cells(cage5h_real,1,2)
   row6=cage_to_cells(cage6h_real,1,1)
   col6=cage_to_cells(cage6h_real,1,2)
   row7=cage_to_cells(cage7h_real,1,1)
   col7=cage_to_cells(cage7h_real,1,2)
   ncells1=cage_no_of_cells(cage1h_real)
   ncells2=cage_no_of_cells(cage2h_real)
   ncells3=cage_no_of_cells(cage3h_real)
   ncells4=cage_no_of_cells(cage4h_real)
   ncells5=cage_no_of_cells(cage5h_real)
   ncells6=cage_no_of_cells(cage6h_real)
   ncells7=cage_no_of_cells(cage7h_real)
   rows(1)=row1
   rows(2)=row2
   rows(3)=row3
   rows(4)=row4
   rows(5)=row5
   rows(6)=row6
   rows(7)=row7
   row_max=0
   row_min=100
   DO iarray=1,7
     IF (row_min .gt. rows(iarray)) THEN
        row_min=rows(iarray)
     END IF
     IF (row_max .lt. rows(iarray)) THEN
        row_max=rows(iarray)
     END IF
   END DO

jloop: DO j=1,nhorizontal_cages
      jcage=horizontal_cages(j)
      IF (jcage .eq. cage1h_real) CYCLE
      IF (jcage .eq. cage2h_real) CYCLE
      IF (jcage .eq. cage3h_real) CYCLE
      IF (jcage .eq. cage4h_real) CYCLE
      IF (jcage .eq. cage5h_real) CYCLE
      IF (jcage .eq. cage6h_real) CYCLE
      IF (jcage .eq. cage7h_real) CYCLE
      ncells8=cage_no_of_cells(jcage)
      row8=cage_to_cells(jcage,1,1)
      col8=cage_to_cells(jcage,1,2)
      IF ((row8 .lt. row_min-1) .OR. (row8 .gt. row_max+1)) CYCLE
      ! check if two consecutive horizontal cages share a vertical cage
      DO k=1,7
        cageh=horizontal_7cages(i,k)
        cageh_real=horizontal_cages(cageh)
        check_connection=0
        CALL check_connectivity(cageh_real,jcage,check_connection)
        IF (check_connection .eq. 1) THEN
          ! We have a 5 cage combination
          new_8cages(1) = cage1h
          new_8cages(2) = cage2h
          new_8cages(3) = cage3h
          new_8cages(4) = cage4h
          new_8cages(5) = cage5h
          new_8cages(6) = cage6h
          new_8cages(7) = cage7h
	  new_8cages(8) = j
          ! check if the combination has already been stored
          check_combo=0
          CALL check_combination_8cages(new_8cages,check_combo)
          IF (check_combo .eq. 0) THEN
            ! update the list of 3 horizontal cages
            nhorizontal_8cages=nhorizontal_8cages+1
            horizontal_8cages(nhorizontal_8cages,1)=cage1h
            horizontal_8cages(nhorizontal_8cages,2)=cage2h
            horizontal_8cages(nhorizontal_8cages,3)=cage3h
            horizontal_8cages(nhorizontal_8cages,4)=cage4h
            horizontal_8cages(nhorizontal_8cages,5)=cage5h
            horizontal_8cages(nhorizontal_8cages,6)=cage6h
            horizontal_8cages(nhorizontal_8cages,7)=cage7h
            horizontal_8cages(nhorizontal_8cages,8)=j
          END IF
          CYCLE jloop
        END IF
      END DO
    END DO jloop
  END DO


END SUBROUTINE Find_8permutation_horizontalcage

!------------------------------------------------------

SUBROUTINE Find_2permutation_verticalcage

  USE global_variables
  IMPLICIT NONE
 
!  INTEGER :: nvertical_2cages
!  INTEGER,DIMENSION(max_2cages,2) :: vertical_2cages

  INTEGER :: i,j,icell,jcell,icage,jcage
  INTEGER :: row1,col1,row2,col2,irow,icol,jrow,jcol
  INTEGER :: ncells1,ncells2
  INTEGER :: cage_temp1,cage_temp2
  INTEGER :: horizontal_cage1,horizontal_cage2
  INTEGER :: check_connection


GO TO 104
!------------- OLD CODE -----------------------------------------
  DO i=1,nvertical_cages-1
   DO j=i+1,nvertical_cages
    nvertical_2cages = nvertical_2cages+1
        IF (nvertical_2cages .ge. max_2cages) THEN
          write(*,*) 'nvertical_2cages is .ge. max_2cages'
          write(*,*) 'increase the size of max_2cages'
          STOP
        END IF

    vertical_2cages(nvertical_2cages,1) = i
    vertical_2cages(nvertical_2cages,2) = j
   END DO
  END DO
!------------ END OLD CODE ------------------------------------
104 CONTINUE 

 DO i=1,nvertical_cages
  icage=vertical_cages(i)
  ncells1=cage_no_of_cells(icage)
  row1=cage_to_cells(icage,1,1)
  col1=cage_to_cells(icage,1,2)
  ! Look for the horizontal cage that have where row2=row1+1 only
jloop:  DO j=1,nvertical_cages
    jcage=vertical_cages(j)
    IF (jcage .eq. icage) CYCLE
    ncells2=cage_no_of_cells(jcage)
    row2=cage_to_cells(jcage,1,1)
    col2=cage_to_cells(jcage,1,2)
    IF (col2 .ne. col1+1) CYCLE
    ! check if two consecutive vertical cages share a horizontal cage
    check_connection=0
    CALL check_connectivity_vertical(icage,jcage,check_connection)
    IF (check_connection .eq. 1) THEN
        ! If execution comes here then that means we have 2 vertical cages
        ! connected with each other
        nvertical_2cages=nvertical_2cages+1
        vertical_2cages(nvertical_2cages,1) = i
        vertical_2cages(nvertical_2cages,2) = j
        CYCLE jloop
    END IF
   END DO jloop
 END DO


END SUBROUTINE Find_2permutation_verticalcage

!-------------------------------------------

SUBROUTINE Find_3permutation_verticalcage

  USE global_variables
  IMPLICIT NONE

!  INTEGER :: nvertical_3cages
!  INTEGER,DIMENSION(max_3cages,3) :: vertical_3cages
  INTEGER,DIMENSION(3) :: new_3cages
  INTEGER :: i,j,k,jcage
  INTEGER :: cage1h,cage2h,cage1h_real,cage2h_real
  INTEGER :: row1,col1,row2,col2,row3,col3
  INTEGER :: ncells1,ncells2,ncells3
  INTEGER :: cage_temp1,cage_temp2
  INTEGER :: horizontal_cage1,horizontal_cage2
  INTEGER :: col_min,col_max
  INTEGER :: check_connection,check_combo
  INTEGER :: cageh,cageh_real


GO TO 105
!--------- OLD CODE ----------------------------------------
  DO i=1,nvertical_cages-2
   DO j=i+1,nvertical_cages-1
    DO k=j+1,nvertical_cages
      nvertical_3cages = nvertical_3cages + 1
        IF (nvertical_3cages .ge. max_3cages) THEN
          write(*,*) 'nvertical_3cages is .ge. max_3cages'
          write(*,*) 'increase the size of max_3cages'
          STOP
        END IF
      vertical_3cages(nvertical_3cages,1) = i
      vertical_3cages(nvertical_3cages,2) = j
      vertical_3cages(nvertical_3cages,3) = k
     END DO
    END DO
   END DO
!--------- END OLD CODE ------------------------------------
105 CONTINUE

 DO i=1,nvertical_2cages
   cage1h=vertical_2cages(i,1)
   cage2h=vertical_2cages(i,2)
   cage1h_real=vertical_cages(cage1h)
   cage2h_real=vertical_cages(cage2h)
   row1=cage_to_cells(cage1h_real,1,1)
   col1=cage_to_cells(cage1h_real,1,2)
   row2=cage_to_cells(cage2h_real,1,1)
   col2=cage_to_cells(cage2h_real,1,2)
   ncells1=cage_no_of_cells(cage1h_real)
   ncells2=cage_no_of_cells(cage2h_real)
   IF (col1 .lt. col2) THEN
     col_min = col1
     col_max = col2
   ELSE
     col_min=col2
     col_max=col1
   END IF
jloop:   DO j=1,nvertical_cages
      jcage=vertical_cages(j)
      IF (jcage .eq. cage1h_real) CYCLE
      IF (jcage .eq. cage2h_real) CYCLE
      ncells3=cage_no_of_cells(jcage)
      row3=cage_to_cells(jcage,1,1)
      col3=cage_to_cells(jcage,1,2)
      IF ((col3 .lt. col_min-1) .OR. (col3 .gt. col_max+1)) CYCLE
      ! check if two consecutive vertical cages share a horizontal cage

!-----------------------------------------------------------------------
      DO k=1,2
	check_connection=0
	cageh=vertical_2cages(i,k)
	cageh_real=vertical_cages(cageh)
	check_connection=0
	CALL check_connectivity_vertical(cageh_real,jcage,check_connection)
	
!---------------------------------------------------------------------------
!      check_connection=0
!      CALL check_connectivity_vertical(cage1h_real,jcage,check_connection)
!      IF (check_connection .eq. 1) THEN
!        ! We have a 3 cage combination
!        new_3cages(1) = cage1h
!        new_3cages(2) = cage2h
!        new_3cages(3) = j
!        ! check if the combination has already been stored
!        check_combo=0
!        CALL check_combination_3cages_vertical(new_3cages,check_combo)
!        IF (check_combo .eq. 0) THEN
!          ! update the list of 3 horizontal cages
!          nvertical_3cages=nvertical_3cages+1
!          vertical_3cages(nvertical_3cages,1)=cage1h
!          vertical_3cages(nvertical_3cages,2)=cage2h
!          vertical_3cages(nvertical_3cages,3)=j
!        END IF
!        CYCLE jloop
!      END IF
!      check_connection=0
!      CALL check_connectivity_vertical(cage2h_real,jcage,check_connection)
!-----------------------------------------------------------------------------
      IF (check_connection .eq. 1) THEN
        ! We have a 3 cage combination
        new_3cages(1) = cage1h
        new_3cages(2) = cage2h
        new_3cages(3) = j
        ! check if the combination has already been stored
        check_combo=0
        CALL check_combination_3cages_vertical(new_3cages,check_combo)
        IF (check_combo .eq. 0) THEN
          ! update the list of 3 horizontal cages
          nvertical_3cages=nvertical_3cages+1
          vertical_3cages(nvertical_3cages,1)=cage1h
          vertical_3cages(nvertical_3cages,2)=cage2h
          vertical_3cages(nvertical_3cages,3)=j
        END IF
        CYCLE jloop
      END IF
     END DO
    END DO jloop
   END DO


END SUBROUTINE Find_3permutation_verticalcage

!-----------------------------------------

SUBROUTINE Find_4permutation_verticalcage

  USE global_variables
  IMPLICIT NONE

!  INTEGER :: nvertical_4cages
!  INTEGER,DIMENSION(max_4cages,4) :: vertical_4cages
  INTEGER :: i,j,k,l,jcage
  INTEGER,DIMENSION(4):: new_4cages
  INTEGER,DIMENSION(3) :: cols
  INTEGER :: cage1h,cage2h,cage1h_real,cage2h_real
  INTEGER :: cage3h,cage3h_real
  INTEGER :: row1,col1,row2,col2,row3,col3,row4,col4
  INTEGER :: ncells1,ncells2,ncells3,ncells4
  INTEGER :: cage_temp1,cage_temp2
  INTEGER :: horizontal_cage1,horizontal_cage2
  INTEGER :: col_min,col_max
  INTEGER :: check_connection,check_combo,iarray
  INTEGER :: cageh,cageh_real

GO TO 106
!-------------- OLD CODE -------------------------------------
  DO i=1,nvertical_cages-3
   DO j=i+1,nvertical_cages-2
    DO k=j+1,nvertical_cages-1
     DO l=k+1,nvertical_cages
       nvertical_4cages = nvertical_4cages + 1
	IF (nvertical_4cages .ge. max_4cages) THEN
	  write(*,*) 'nvertical_4cages is .ge. max_4cages'
	  write(*,*) 'increase the size of max_4cages'
	  STOP
	END IF
       vertical_4cages(nvertical_4cages,1) = i
       vertical_4cages(nvertical_4cages,2) = j
       vertical_4cages(nvertical_4cages,3) = k
       vertical_4cages(nvertical_4cages,4) = l
      END DO
     END DO
    END DO
   END DO
!------------ END OLD CODE --------------------------------
106 CONTINUE

 DO i=1,nvertical_3cages
   cage1h=vertical_3cages(i,1)
   cage2h=vertical_3cages(i,2)
   cage3h=vertical_3cages(i,3)
   cage1h_real=vertical_cages(cage1h)
   cage2h_real=vertical_cages(cage2h)
   cage3h_real=vertical_cages(cage3h)
   row1=cage_to_cells(cage1h_real,1,1)
   col1=cage_to_cells(cage1h_real,1,2)
   row2=cage_to_cells(cage2h_real,1,1)
   col2=cage_to_cells(cage2h_real,1,2)
   row3=cage_to_cells(cage3h_real,1,1)
   col3=cage_to_cells(cage3h_real,1,2)
   ncells1=cage_no_of_cells(cage1h_real)
   ncells2=cage_no_of_cells(cage2h_real)
   ncells3=cage_no_of_cells(cage3h_real)
   cols(1)=col1
   cols(2)=col2
   cols(3)=col3
   col_max=0
   col_min=100
   DO iarray=1,3
     IF (col_min .gt. cols(iarray)) THEN
        col_min=cols(iarray)
     END IF
     IF (col_max .lt. cols(iarray)) THEN
        col_max=cols(iarray)
     END IF
   END DO
jloop: DO j=1,nvertical_cages
      jcage=vertical_cages(j)
      IF (jcage .eq. cage1h_real) CYCLE
      IF (jcage .eq. cage2h_real) CYCLE
      IF (jcage .eq. cage3h_real) CYCLE
      ncells4=cage_no_of_cells(jcage)
      row4=cage_to_cells(jcage,1,1)
      col4=cage_to_cells(jcage,1,2)
      IF ((col4 .lt. col_min-1) .OR. (col4 .gt. col_max+1)) CYCLE
      ! check if two consecutive vertical cages share a horizontal cage
      DO k=1,3
	cageh=vertical_3cages(i,k)
	cageh_real=vertical_cages(cageh)
        check_connection=0
        CALL check_connectivity_vertical(cageh_real,jcage,check_connection)
        IF (check_connection .eq. 1) THEN
          ! We have a 4 cage combination
          new_4cages(1) = cage1h
          new_4cages(2) = cage2h
          new_4cages(3) = cage3h
          new_4cages(4) = j
          ! check if the combination has already been stored
          check_combo=0
          CALL check_combination_4cages_vertical(new_4cages,check_combo)
          IF (check_combo .eq. 0) THEN
            ! update the list of 3 vertical cages
            nvertical_4cages=nvertical_4cages+1
            vertical_4cages(nvertical_4cages,1)=cage1h
            vertical_4cages(nvertical_4cages,2)=cage2h
            vertical_4cages(nvertical_4cages,3)=cage3h
            vertical_4cages(nvertical_4cages,4)=j
          END IF
          CYCLE jloop
        END IF
      END DO
    END DO jloop
  END DO


END SUBROUTINE Find_4permutation_verticalcage

!----------------------------------------

SUBROUTINE Find_5permutation_verticalcage

  USE global_variables
  IMPLICIT NONE

  INTEGER :: i,j,k,l,jcage
  INTEGER,DIMENSION(5):: new_5cages
  INTEGER,DIMENSION(4) :: cols
  INTEGER :: cage1h,cage2h,cage1h_real,cage2h_real
  INTEGER :: cage3h,cage3h_real,cage4h,cage4h_real
  INTEGER :: row1,col1,row2,col2,row3,col3,row4,col4,row5,col5
  INTEGER :: ncells1,ncells2,ncells3,ncells4,ncells5
  INTEGER :: cage_temp1,cage_temp2
  INTEGER :: horizontal_cage1,horizontal_cage2
  INTEGER :: col_min,col_max
  INTEGER :: check_connection,check_combo,iarray
  INTEGER :: cageh,cageh_real

 DO i=1,nvertical_4cages
   cage1h=vertical_4cages(i,1)
   cage2h=vertical_4cages(i,2)
   cage3h=vertical_4cages(i,3)
   cage4h=vertical_4cages(i,4)
   cage1h_real=vertical_cages(cage1h)
   cage2h_real=vertical_cages(cage2h)
   cage3h_real=vertical_cages(cage3h)
   cage4h_real=vertical_cages(cage4h)
   row1=cage_to_cells(cage1h_real,1,1)
   col1=cage_to_cells(cage1h_real,1,2)
   row2=cage_to_cells(cage2h_real,1,1)
   col2=cage_to_cells(cage2h_real,1,2)
   row3=cage_to_cells(cage3h_real,1,1)
   col3=cage_to_cells(cage3h_real,1,2)
   row4=cage_to_cells(cage4h_real,1,1)
   col4=cage_to_cells(cage4h_real,1,2)
   ncells1=cage_no_of_cells(cage1h_real)
   ncells2=cage_no_of_cells(cage2h_real)
   ncells3=cage_no_of_cells(cage3h_real)
   ncells4=cage_no_of_cells(cage4h_real)
   cols(1)=col1
   cols(2)=col2
   cols(3)=col3
   cols(4)=col4
   col_max=0
   col_min=100
   DO iarray=1,4
     IF (col_min .gt. cols(iarray)) THEN
        col_min=cols(iarray)
     END IF
     IF (col_max .lt. cols(iarray)) THEN
        col_max=cols(iarray)
     END IF
   END DO

jloop: DO j=1,nvertical_cages
      jcage=vertical_cages(j)
      IF (jcage .eq. cage1h_real) CYCLE
      IF (jcage .eq. cage2h_real) CYCLE
      IF (jcage .eq. cage3h_real) CYCLE
      IF (jcage .eq. cage4h_real) CYCLE
      ncells5=cage_no_of_cells(jcage)
      row5=cage_to_cells(jcage,1,1)
      col5=cage_to_cells(jcage,1,2)
      IF ((col5 .lt. col_min-1) .OR. (col5 .gt. col_max+1)) CYCLE
      ! check if two consecutive vertical cages share a horizontal cage
      DO k=1,4
        cageh=vertical_4cages(i,k)
        cageh_real=vertical_cages(cageh)
        check_connection=0
        CALL check_connectivity_vertical(cageh_real,jcage,check_connection)
        IF (check_connection .eq. 1) THEN
          ! We have a 4 cage combination
          new_5cages(1) = cage1h
          new_5cages(2) = cage2h
          new_5cages(3) = cage3h
          new_5cages(4) = cage4h
	  new_5cages(5) = j
          ! check if the combination has already been stored
          check_combo=0
          CALL check_combination_5cages_vertical(new_5cages,check_combo)
          IF (check_combo .eq. 0) THEN
            ! update the list of 3 vertical cages
            nvertical_5cages=nvertical_5cages+1
            vertical_5cages(nvertical_5cages,1)=cage1h
            vertical_5cages(nvertical_5cages,2)=cage2h
            vertical_5cages(nvertical_5cages,3)=cage3h
            vertical_5cages(nvertical_5cages,4)=cage4h
            vertical_5cages(nvertical_5cages,5)=j
          END IF
          CYCLE jloop
        END IF
      END DO
    END DO jloop
  END DO


END SUBROUTINE Find_5permutation_verticalcage

!----------------------------------------

SUBROUTINE Find_6permutation_verticalcage

  USE global_variables
  IMPLICIT NONE

  INTEGER :: i,j,k,l,jcage
  INTEGER,DIMENSION(6):: new_6cages
  INTEGER,DIMENSION(5) :: cols
  INTEGER :: cage1h,cage2h,cage1h_real,cage2h_real
  INTEGER :: cage3h,cage3h_real,cage4h,cage4h_real
  INTEGER :: cage5h,cage5h_real
  INTEGER :: row1,col1,row2,col2,row3,col3,row4,col4,row5,col5
  INTEGER :: row6,col6
  INTEGER :: ncells1,ncells2,ncells3,ncells4,ncells5,ncells6
  INTEGER :: cage_temp1,cage_temp2
  INTEGER :: horizontal_cage1,horizontal_cage2
  INTEGER :: col_min,col_max
  INTEGER :: check_connection,check_combo,iarray
  INTEGER :: cageh,cageh_real

 DO i=1,nvertical_5cages
   cage1h=vertical_5cages(i,1)
   cage2h=vertical_5cages(i,2)
   cage3h=vertical_5cages(i,3)
   cage4h=vertical_5cages(i,4)
   cage5h=vertical_5cages(i,5)
   cage1h_real=vertical_cages(cage1h)
   cage2h_real=vertical_cages(cage2h)
   cage3h_real=vertical_cages(cage3h)
   cage4h_real=vertical_cages(cage4h)
   cage5h_real=vertical_cages(cage5h)
   row1=cage_to_cells(cage1h_real,1,1)
   col1=cage_to_cells(cage1h_real,1,2)
   row2=cage_to_cells(cage2h_real,1,1)
   col2=cage_to_cells(cage2h_real,1,2)
   row3=cage_to_cells(cage3h_real,1,1)
   col3=cage_to_cells(cage3h_real,1,2)
   row4=cage_to_cells(cage4h_real,1,1)
   col4=cage_to_cells(cage4h_real,1,2)
   row5=cage_to_cells(cage5h_real,1,1)
   col5=cage_to_cells(cage5h_real,1,2)
   ncells1=cage_no_of_cells(cage1h_real)
   ncells2=cage_no_of_cells(cage2h_real)
   ncells3=cage_no_of_cells(cage3h_real)
   ncells4=cage_no_of_cells(cage4h_real)
   ncells5=cage_no_of_cells(cage5h_real)
   cols(1)=col1
   cols(2)=col2
   cols(3)=col3
   cols(4)=col4
   cols(5)=col5
   col_max=0
   col_min=100
   DO iarray=1,5
     IF (col_min .gt. cols(iarray)) THEN
        col_min=cols(iarray)
     END IF
     IF (col_max .lt. cols(iarray)) THEN
        col_max=cols(iarray)
     END IF
   END DO

jloop: DO j=1,nvertical_cages
      jcage=vertical_cages(j)
      IF (jcage .eq. cage1h_real) CYCLE
      IF (jcage .eq. cage2h_real) CYCLE
      IF (jcage .eq. cage3h_real) CYCLE
      IF (jcage .eq. cage4h_real) CYCLE
      IF (jcage .eq. cage5h_real) CYCLE
      ncells6=cage_no_of_cells(jcage)
      row6=cage_to_cells(jcage,1,1)
      col6=cage_to_cells(jcage,1,2)
      IF ((col6 .lt. col_min-1) .OR. (col6 .gt. col_max+1)) CYCLE
      ! check if two consecutive vertical cages share a horizontal cage
      DO k=1,5
        cageh=vertical_5cages(i,k)
        cageh_real=vertical_cages(cageh)
        check_connection=0
        CALL check_connectivity_vertical(cageh_real,jcage,check_connection)
        IF (check_connection .eq. 1) THEN
          ! We have a 4 cage combination
          new_6cages(1) = cage1h
          new_6cages(2) = cage2h
          new_6cages(3) = cage3h
          new_6cages(4) = cage4h
          new_6cages(5) = cage5h
          new_6cages(6) = j
          ! check if the combination has already been stored
          check_combo=0
          CALL check_combination_6cages_vertical(new_6cages,check_combo)
          IF (check_combo .eq. 0) THEN
            ! update the list of 3 vertical cages
            nvertical_6cages=nvertical_6cages+1
            vertical_6cages(nvertical_6cages,1)=cage1h
            vertical_6cages(nvertical_6cages,2)=cage2h
            vertical_6cages(nvertical_6cages,3)=cage3h
            vertical_6cages(nvertical_6cages,4)=cage4h
            vertical_6cages(nvertical_6cages,5)=cage5h
            vertical_6cages(nvertical_6cages,6)=j
          END IF
          CYCLE jloop
        END IF
      END DO
    END DO jloop
  END DO


END SUBROUTINE Find_6permutation_verticalcage

!----------------------------------------

SUBROUTINE Find_7permutation_verticalcage

  USE global_variables
  IMPLICIT NONE

  INTEGER :: i,j,k,l,jcage
  INTEGER,DIMENSION(7):: new_7cages
  INTEGER,DIMENSION(6) :: cols
  INTEGER :: cage1h,cage2h,cage1h_real,cage2h_real
  INTEGER :: cage3h,cage3h_real,cage4h,cage4h_real
  INTEGER :: cage5h,cage5h_real,cage6h,cage6h_real
  INTEGER :: row1,col1,row2,col2,row3,col3,row4,col4,row5,col5
  INTEGER :: row6,col6,row7,col7
  INTEGER :: ncells1,ncells2,ncells3,ncells4,ncells5,ncells6,ncells7
  INTEGER :: cage_temp1,cage_temp2
  INTEGER :: horizontal_cage1,horizontal_cage2
  INTEGER :: col_min,col_max
  INTEGER :: check_connection,check_combo,iarray
  INTEGER :: cageh,cageh_real

 DO i=1,nvertical_6cages
   cage1h=vertical_6cages(i,1)
   cage2h=vertical_6cages(i,2)
   cage3h=vertical_6cages(i,3)
   cage4h=vertical_6cages(i,4)
   cage5h=vertical_6cages(i,5)
   cage6h=vertical_6cages(i,6)
   cage1h_real=vertical_cages(cage1h)
   cage2h_real=vertical_cages(cage2h)
   cage3h_real=vertical_cages(cage3h)
   cage4h_real=vertical_cages(cage4h)
   cage5h_real=vertical_cages(cage5h)
   cage6h_real=vertical_cages(cage6h)
   row1=cage_to_cells(cage1h_real,1,1)
   col1=cage_to_cells(cage1h_real,1,2)
   row2=cage_to_cells(cage2h_real,1,1)
   col2=cage_to_cells(cage2h_real,1,2)
   row3=cage_to_cells(cage3h_real,1,1)
   col3=cage_to_cells(cage3h_real,1,2)
   row4=cage_to_cells(cage4h_real,1,1)
   col4=cage_to_cells(cage4h_real,1,2)
   row5=cage_to_cells(cage5h_real,1,1)
   col5=cage_to_cells(cage5h_real,1,2)
   row6=cage_to_cells(cage6h_real,1,1)
   col6=cage_to_cells(cage6h_real,1,2)
   ncells1=cage_no_of_cells(cage1h_real)
   ncells2=cage_no_of_cells(cage2h_real)
   ncells3=cage_no_of_cells(cage3h_real)
   ncells4=cage_no_of_cells(cage4h_real)
   ncells5=cage_no_of_cells(cage5h_real)
   ncells6=cage_no_of_cells(cage6h_real)
   cols(1)=col1
   cols(2)=col2
   cols(3)=col3
   cols(4)=col4
   cols(5)=col5
   cols(6)=col6
   col_max=0
   col_min=100
   DO iarray=1,6
     IF (col_min .gt. cols(iarray)) THEN
        col_min=cols(iarray)
     END IF
     IF (col_max .lt. cols(iarray)) THEN
        col_max=cols(iarray)
     END IF
   END DO

jloop: DO j=1,nvertical_cages
      jcage=vertical_cages(j)
      IF (jcage .eq. cage1h_real) CYCLE
      IF (jcage .eq. cage2h_real) CYCLE
      IF (jcage .eq. cage3h_real) CYCLE
      IF (jcage .eq. cage4h_real) CYCLE
      IF (jcage .eq. cage5h_real) CYCLE
      IF (jcage .eq. cage6h_real) CYCLE
      ncells7=cage_no_of_cells(jcage)
      row7=cage_to_cells(jcage,1,1)
      col7=cage_to_cells(jcage,1,2)
      IF ((col7 .lt. col_min-1) .OR. (col7 .gt. col_max+1)) CYCLE
      ! check if two consecutive vertical cages share a horizontal cage
      DO k=1,6
        cageh=vertical_6cages(i,k)
        cageh_real=vertical_cages(cageh)
        check_connection=0
        CALL check_connectivity_vertical(cageh_real,jcage,check_connection)
        IF (check_connection .eq. 1) THEN
          ! We have a 4 cage combination
          new_7cages(1) = cage1h
          new_7cages(2) = cage2h
          new_7cages(3) = cage3h
          new_7cages(4) = cage4h
          new_7cages(5) = cage5h
          new_7cages(6) = cage6h
          new_7cages(7) = j
          ! check if the combination has already been stored
          check_combo=0
          CALL check_combination_7cages_vertical(new_7cages,check_combo)
          IF (check_combo .eq. 0) THEN
            ! update the list of 3 vertical cages
            nvertical_7cages=nvertical_7cages+1
            vertical_7cages(nvertical_7cages,1)=cage1h
            vertical_7cages(nvertical_7cages,2)=cage2h
            vertical_7cages(nvertical_7cages,3)=cage3h
            vertical_7cages(nvertical_7cages,4)=cage4h
            vertical_7cages(nvertical_7cages,5)=cage5h
            vertical_7cages(nvertical_7cages,6)=cage6h
            vertical_7cages(nvertical_7cages,7)=j
          END IF
          CYCLE jloop
        END IF
      END DO
    END DO jloop
  END DO


END SUBROUTINE Find_7permutation_verticalcage

!----------------------------------------

SUBROUTINE Find_8permutation_verticalcage

  USE global_variables
  IMPLICIT NONE

  INTEGER :: i,j,k,l,jcage
  INTEGER,DIMENSION(8):: new_8cages
  INTEGER,DIMENSION(7) :: cols
  INTEGER :: cage1h,cage2h,cage1h_real,cage2h_real
  INTEGER :: cage3h,cage3h_real,cage4h,cage4h_real
  INTEGER :: cage5h,cage5h_real,cage6h,cage6h_real
  INTEGER :: cage7h,cage7h_real
  INTEGER :: row1,col1,row2,col2,row3,col3,row4,col4,row5,col5
  INTEGER :: row6,col6,row7,col7,row8,col8
  INTEGER :: ncells1,ncells2,ncells3,ncells4,ncells5,ncells6,ncells7,ncells8
  INTEGER :: cage_temp1,cage_temp2
  INTEGER :: horizontal_cage1,horizontal_cage2
  INTEGER :: col_min,col_max
  INTEGER :: check_connection,check_combo,iarray
  INTEGER :: cageh,cageh_real

 DO i=1,nvertical_7cages
   cage1h=vertical_7cages(i,1)
   cage2h=vertical_7cages(i,2)
   cage3h=vertical_7cages(i,3)
   cage4h=vertical_7cages(i,4)
   cage5h=vertical_7cages(i,5)
   cage6h=vertical_7cages(i,6)
   cage7h=vertical_7cages(i,7)
   cage1h_real=vertical_cages(cage1h)
   cage2h_real=vertical_cages(cage2h)
   cage3h_real=vertical_cages(cage3h)
   cage4h_real=vertical_cages(cage4h)
   cage5h_real=vertical_cages(cage5h)
   cage6h_real=vertical_cages(cage6h)
   cage7h_real=vertical_cages(cage7h)
   row1=cage_to_cells(cage1h_real,1,1)
   col1=cage_to_cells(cage1h_real,1,2)
   row2=cage_to_cells(cage2h_real,1,1)
   col2=cage_to_cells(cage2h_real,1,2)
   row3=cage_to_cells(cage3h_real,1,1)
   col3=cage_to_cells(cage3h_real,1,2)
   row4=cage_to_cells(cage4h_real,1,1)
   col4=cage_to_cells(cage4h_real,1,2)
   row5=cage_to_cells(cage5h_real,1,1)
   col5=cage_to_cells(cage5h_real,1,2)
   row6=cage_to_cells(cage6h_real,1,1)
   col6=cage_to_cells(cage6h_real,1,2)
   row7=cage_to_cells(cage7h_real,1,1)
   col7=cage_to_cells(cage7h_real,1,2)
   ncells1=cage_no_of_cells(cage1h_real)
   ncells2=cage_no_of_cells(cage2h_real)
   ncells3=cage_no_of_cells(cage3h_real)
   ncells4=cage_no_of_cells(cage4h_real)
   ncells5=cage_no_of_cells(cage5h_real)
   ncells6=cage_no_of_cells(cage6h_real)
   ncells7=cage_no_of_cells(cage7h_real)
   cols(1)=col1
   cols(2)=col2
   cols(3)=col3
   cols(4)=col4
   cols(5)=col5
   cols(6)=col6
   cols(7)=col7
   col_max=0
   col_min=100
   DO iarray=1,7
     IF (col_min .gt. cols(iarray)) THEN
        col_min=cols(iarray)
     END IF
     IF (col_max .lt. cols(iarray)) THEN
        col_max=cols(iarray)
     END IF
   END DO

jloop: DO j=1,nvertical_cages
      jcage=vertical_cages(j)
      IF (jcage .eq. cage1h_real) CYCLE
      IF (jcage .eq. cage2h_real) CYCLE
      IF (jcage .eq. cage3h_real) CYCLE
      IF (jcage .eq. cage4h_real) CYCLE
      IF (jcage .eq. cage5h_real) CYCLE
      IF (jcage .eq. cage6h_real) CYCLE
      IF (jcage .eq. cage7h_real) CYCLE
      ncells8=cage_no_of_cells(jcage)
      row8=cage_to_cells(jcage,1,1)
      col8=cage_to_cells(jcage,1,2)
      IF ((col8 .lt. col_min-1) .OR. (col8 .gt. col_max+1)) CYCLE
      ! check if two consecutive vertical cages share a horizontal cage
      DO k=1,7
        cageh=vertical_7cages(i,k)
        cageh_real=vertical_cages(cageh)
        check_connection=0
        CALL check_connectivity_vertical(cageh_real,jcage,check_connection)
        IF (check_connection .eq. 1) THEN
          ! We have a 4 cage combination
          new_8cages(1) = cage1h
          new_8cages(2) = cage2h
          new_8cages(3) = cage3h
          new_8cages(4) = cage4h
          new_8cages(5) = cage5h
          new_8cages(6) = cage6h
          new_8cages(7) = cage7h
          new_8cages(8) = j
          ! check if the combination has already been stored
          check_combo=0
          CALL check_combination_8cages_vertical(new_8cages,check_combo)
          IF (check_combo .eq. 0) THEN
            ! update the list of 3 vertical cages
            nvertical_8cages=nvertical_8cages+1
            vertical_8cages(nvertical_8cages,1)=cage1h
            vertical_8cages(nvertical_8cages,2)=cage2h
            vertical_8cages(nvertical_8cages,3)=cage3h
            vertical_8cages(nvertical_8cages,4)=cage4h
            vertical_8cages(nvertical_8cages,5)=cage5h
            vertical_8cages(nvertical_8cages,6)=cage6h
            vertical_8cages(nvertical_8cages,7)=cage7h
            vertical_8cages(nvertical_8cages,8)=j
          END IF
          CYCLE jloop
        END IF
      END DO
    END DO jloop
  END DO


END SUBROUTINE Find_8permutation_verticalcage

!----------------------------------------

SUBROUTINE check_connectivity(icage,jcage,check_connection)

  USE global_variables
  IMPLICIT NONE

  INTEGER :: icage,jcage,check_connection
  INTEGER :: row1,col1,row2,col2,irow,icol,jrow,jcol
  INTEGER :: ncells1,ncells2,icell,jcell
  INTEGER :: cage_temp1,cage_temp2
  INTEGER :: vertical_cage1,vertical_cage2
 
    check_connection=0 
    ncells1=cage_no_of_cells(icage)
    ncells2=cage_no_of_cells(jcage)
    DO icell=1,ncells1
      irow=cage_to_cells(icage,icell,1)
      icol=cage_to_cells(icage,icell,2)
      cage_temp1=cell_to_cage(irow,icol,1)
      cage_temp2=cell_to_cage(irow,icol,2)
      IF (cage_temp1 .eq. icage) THEN
        vertical_cage1 = cage_temp2
      ELSE IF (cage_temp2 .eq. icage) THEN
        vertical_cage1 = cage_temp1
      ELSE
        write(*,*) 'SOMETHING IS wrong: Find_2permutation_horizontalcage -1'
        STOP
      END IF
      DO jcell=1,ncells2
        jrow=cage_to_cells(jcage,jcell,1)
        jcol=cage_to_cells(jcage,jcell,2)
        ! check if these two cells share a vertical cage
        cage_temp1=cell_to_cage(jrow,jcol,1)
        cage_temp2=cell_to_cage(jrow,jcol,2)
        IF (cage_temp1 .eq. jcage) THEN
          vertical_cage2 = cage_temp2
        ELSE IF (cage_temp2 .eq. jcage) THEN
          vertical_cage2 = cage_temp1
        ELSE
          write(*,*) 'SOMETHING IS wrong: Find_2permutation_horizontalcage -2'
        END IF
	IF (vertical_cage1 .eq. vertical_cage2) THEN
	  IF ((abs(irow-jrow) .eq. 1) .OR. (abs(jrow-irow) .eq. 1)) THEN
	    check_connection = 1
	    RETURN
	  END IF
	END IF
      END DO
    END DO
 


END SUBROUTINE check_connectivity
!-----------------------------------------

SUBROUTINE check_combination_3cages(new_3cages,check_combo)

  USE global_variables
  IMPLICIT NONE

  INTEGER,DIMENSION(3) :: new_3cages
  INTEGER :: check_combo
  INTEGER :: cage1,cage2,count
  INTEGER :: i,j,k
  
  check_combo=0
  DO i=1,nhorizontal_3cages
    count=0
jloop:    DO j=1,3
      cage1=horizontal_3cages(i,j)
      DO k=1,3
        cage2=new_3cages(k)
	IF (cage1 .eq. cage2) THEN
	  count=count+1
	  CYCLE jloop
	END IF
      END DO
    END DO jloop
    IF (count .eq. 3) THEN
      check_combo=1
      RETURN
    END IF
  END DO
	  

END SUBROUTINE check_combination_3cages
!-----------------------------------------

SUBROUTINE check_combination_4cages(new_4cages,check_combo)

  USE global_variables
  IMPLICIT NONE

  INTEGER,DIMENSION(4) :: new_4cages
  INTEGER :: check_combo
  INTEGER :: cage1,cage2,count
  INTEGER :: i,j,k

  check_combo=0
  DO i=1,nhorizontal_4cages
    count=0
jloop:    DO j=1,4
      cage1=horizontal_4cages(i,j)
      DO k=1,4
        cage2=new_4cages(k)
        IF (cage1 .eq. cage2) THEN
          count=count+1
          CYCLE jloop
        END IF
      END DO
    END DO jloop
    IF (count .eq. 4) THEN
      check_combo=1
      RETURN
    END IF
  END DO


END SUBROUTINE check_combination_4cages
!-----------------------------------------

SUBROUTINE check_connectivity_vertical(icage,jcage,check_connection)

  USE global_variables
  IMPLICIT NONE

  INTEGER :: icage,jcage,check_connection
  INTEGER :: row1,col1,row2,col2,irow,icol,jrow,jcol
  INTEGER :: ncells1,ncells2,icell,jcell
  INTEGER :: cage_temp1,cage_temp2
  INTEGER :: horizontal_cage1,horizontal_cage2

    check_connection=0
    ncells1=cage_no_of_cells(icage)
    ncells2=cage_no_of_cells(jcage)
    DO icell=1,ncells1
      irow=cage_to_cells(icage,icell,1)
      icol=cage_to_cells(icage,icell,2)
      cage_temp1=cell_to_cage(irow,icol,1)
      cage_temp2=cell_to_cage(irow,icol,2)
      IF (cage_temp1 .eq. icage) THEN
        horizontal_cage1 = cage_temp2
      ELSE IF (cage_temp2 .eq. icage) THEN
        horizontal_cage1 = cage_temp1
      ELSE
        write(*,*) 'SOMETHING IS wrong: Find_2permutation_horizontalcage -1'
        STOP
      END IF
      DO jcell=1,ncells2
        jrow=cage_to_cells(jcage,jcell,1)
        jcol=cage_to_cells(jcage,jcell,2)
        ! check if these two cells share a vertical cage
        cage_temp1=cell_to_cage(jrow,jcol,1)
        cage_temp2=cell_to_cage(jrow,jcol,2)
        IF (cage_temp1 .eq. jcage) THEN
          horizontal_cage2 = cage_temp2
        ELSE IF (cage_temp2 .eq. jcage) THEN
          horizontal_cage2 = cage_temp1
        ELSE
          write(*,*) 'SOMETHING IS wrong: Find_2permutation_horizontalcage -2'
        END IF
        IF (horizontal_cage1 .eq. horizontal_cage2) THEN
          IF ((abs(icol-jcol) .eq. 1) .OR. (abs(jcol-icol) .eq. 1)) THEN
            check_connection = 1
            RETURN
          END IF
        END IF
      END DO
    END DO


END SUBROUTINE check_connectivity_vertical
!-----------------------------------------

SUBROUTINE check_combination_3cages_vertical(new_3cages,check_combo)

  USE global_variables
  IMPLICIT NONE

  INTEGER,DIMENSION(3) :: new_3cages
  INTEGER :: check_combo
  INTEGER :: cage1,cage2,count
  INTEGER :: i,j,k

  check_combo=0
  DO i=1,nvertical_3cages
    count=0
jloop:    DO j=1,3
      cage1=vertical_3cages(i,j)
      DO k=1,3
        cage2=new_3cages(k)
        IF (cage1 .eq. cage2) THEN
          count=count+1
          CYCLE jloop
        END IF
      END DO
    END DO jloop
    IF (count .eq. 3) THEN
      check_combo=1
      RETURN
    END IF
  END DO


END SUBROUTINE check_combination_3cages_vertical
!-----------------------------------------

SUBROUTINE check_combination_4cages_vertical(new_4cages,check_combo)

  USE global_variables
  IMPLICIT NONE

  INTEGER,DIMENSION(4) :: new_4cages
  INTEGER :: check_combo
  INTEGER :: cage1,cage2,count
  INTEGER :: i,j,k

  check_combo=0
  DO i=1,nvertical_4cages
    count=0
jloop:    DO j=1,4
      cage1=vertical_4cages(i,j)
      DO k=1,4
        cage2=new_4cages(k)
        IF (cage1 .eq. cage2) THEN
          count=count+1
          CYCLE jloop
        END IF
      END DO
    END DO jloop
    IF (count .eq. 4) THEN
      check_combo=1
      RETURN
    END IF
  END DO


END SUBROUTINE check_combination_4cages_vertical
!-----------------------------------------

SUBROUTINE check_combination_5cages(new_5cages,check_combo)

  USE global_variables
  IMPLICIT NONE

  INTEGER,DIMENSION(5) :: new_5cages
  INTEGER :: check_combo
  INTEGER :: cage1,cage2,count
  INTEGER :: i,j,k

  check_combo=0
  DO i=1,nhorizontal_5cages
    count=0
jloop:    DO j=1,5
      cage1=horizontal_5cages(i,j)
      DO k=1,5
        cage2=new_5cages(k)
        IF (cage1 .eq. cage2) THEN
          count=count+1
          CYCLE jloop
        END IF
      END DO
    END DO jloop
    IF (count .eq. 5) THEN
      check_combo=1
      RETURN
    END IF
  END DO


END SUBROUTINE check_combination_5cages
!-----------------------------------------

SUBROUTINE check_combination_6cages(new_6cages,check_combo)

  USE global_variables
  IMPLICIT NONE

  INTEGER,DIMENSION(6) :: new_6cages
  INTEGER :: check_combo
  INTEGER :: cage1,cage2,count
  INTEGER :: i,j,k

  check_combo=0
  DO i=1,nhorizontal_6cages
    count=0
jloop:    DO j=1,6
      cage1=horizontal_6cages(i,j)
      DO k=1,6
        cage2=new_6cages(k)
        IF (cage1 .eq. cage2) THEN
          count=count+1
          CYCLE jloop
        END IF
      END DO
    END DO jloop
    IF (count .eq. 6) THEN
      check_combo=1
      RETURN
    END IF
  END DO


END SUBROUTINE check_combination_6cages
!-----------------------------------------

SUBROUTINE check_combination_7cages(new_7cages,check_combo)

  USE global_variables
  IMPLICIT NONE

  INTEGER,DIMENSION(7) :: new_7cages
  INTEGER :: check_combo
  INTEGER :: cage1,cage2,count
  INTEGER :: i,j,k

  check_combo=0
  DO i=1,nhorizontal_7cages
    count=0
jloop:    DO j=1,7
      cage1=horizontal_7cages(i,j)
      DO k=1,7
        cage2=new_7cages(k)
        IF (cage1 .eq. cage2) THEN
          count=count+1
          CYCLE jloop
        END IF
      END DO
    END DO jloop
    IF (count .eq. 7) THEN
      check_combo=1
      RETURN
    END IF
  END DO


END SUBROUTINE check_combination_7cages
!-----------------------------------------

SUBROUTINE check_combination_8cages(new_8cages,check_combo)

  USE global_variables
  IMPLICIT NONE

  INTEGER,DIMENSION(8) :: new_8cages
  INTEGER :: check_combo
  INTEGER :: cage1,cage2,count
  INTEGER :: i,j,k

  check_combo=0
  DO i=1,nhorizontal_8cages
    count=0
jloop:    DO j=1,8
      cage1=horizontal_8cages(i,j)
      DO k=1,8
        cage2=new_8cages(k)
        IF (cage1 .eq. cage2) THEN
          count=count+1
          CYCLE jloop
        END IF
      END DO
    END DO jloop
    IF (count .eq. 8) THEN
      check_combo=1
      RETURN
    END IF
  END DO


END SUBROUTINE check_combination_8cages
!-----------------------------------------

SUBROUTINE check_combination_5cages_vertical(new_5cages,check_combo)

  USE global_variables
  IMPLICIT NONE

  INTEGER,DIMENSION(5) :: new_5cages
  INTEGER :: check_combo
  INTEGER :: cage1,cage2,count
  INTEGER :: i,j,k

  check_combo=0
  DO i=1,nvertical_5cages
    count=0
jloop:    DO j=1,5
      cage1=vertical_5cages(i,j)
      DO k=1,5
        cage2=new_5cages(k)
        IF (cage1 .eq. cage2) THEN
          count=count+1
          CYCLE jloop
        END IF
      END DO
    END DO jloop
    IF (count .eq. 5) THEN
      check_combo=1
      RETURN
    END IF
  END DO


END SUBROUTINE check_combination_5cages_vertical
!-----------------------------------------

SUBROUTINE check_combination_6cages_vertical(new_6cages,check_combo)

  USE global_variables
  IMPLICIT NONE

  INTEGER,DIMENSION(6) :: new_6cages
  INTEGER :: check_combo
  INTEGER :: cage1,cage2,count
  INTEGER :: i,j,k

  check_combo=0
  DO i=1,nvertical_6cages
    count=0
jloop:    DO j=1,6
      cage1=vertical_6cages(i,j)
      DO k=1,6
        cage2=new_6cages(k)
        IF (cage1 .eq. cage2) THEN
          count=count+1
          CYCLE jloop
        END IF
      END DO
    END DO jloop
    IF (count .eq. 6) THEN
      check_combo=1
      RETURN
    END IF
  END DO


END SUBROUTINE check_combination_6cages_vertical
!-----------------------------------------

SUBROUTINE check_combination_7cages_vertical(new_7cages,check_combo)

  USE global_variables
  IMPLICIT NONE

  INTEGER,DIMENSION(7) :: new_7cages
  INTEGER :: check_combo
  INTEGER :: cage1,cage2,count
  INTEGER :: i,j,k

  check_combo=0
  DO i=1,nvertical_7cages
    count=0
jloop:    DO j=1,7
      cage1=vertical_7cages(i,j)
      DO k=1,7
        cage2=new_7cages(k)
        IF (cage1 .eq. cage2) THEN
          count=count+1
          CYCLE jloop
        END IF
      END DO
    END DO jloop
    IF (count .eq. 7) THEN
      check_combo=1
      RETURN
    END IF
  END DO


END SUBROUTINE check_combination_7cages_vertical
!-----------------------------------------

SUBROUTINE check_combination_8cages_vertical(new_8cages,check_combo)

  USE global_variables
  IMPLICIT NONE

  INTEGER,DIMENSION(8) :: new_8cages
  INTEGER :: check_combo
  INTEGER :: cage1,cage2,count
  INTEGER :: i,j,k

  check_combo=0
  DO i=1,nvertical_8cages
    count=0
jloop:    DO j=1,8
      cage1=vertical_8cages(i,j)
      DO k=1,8
        cage2=new_8cages(k)
        IF (cage1 .eq. cage2) THEN
          count=count+1
          CYCLE jloop
        END IF
      END DO
    END DO jloop
    IF (count .eq. 8) THEN
      check_combo=1
      RETURN
    END IF
  END DO


END SUBROUTINE check_combination_8cages_vertical
!-----------------------------------------

SUBROUTINE naked_pairs_nonoverlap_horizontal(cageh_real,overlap,sum_horizontal)

  USE global_variables
  IMPLICIT NONE

  INTEGER :: cageh_real,sum_horizontal
  INTEGER,DIMENSION(max_grid_size1,max_grid_size2,3) :: overlap  
  INTEGER,DIMENSION(naked_list_max,4,2) :: naked_list
  INTEGER,DIMENSION(9) :: naked_values,nvalues
  INTEGER :: inaked,sum_naked,naked_count,ivalue
  INTEGER :: cell_logic_number,irow,icol,inumber
  INTEGER :: row1,col1,row2,col2,row3,col3,row4,col4
  INTEGER :: cell_numbers1,cell_numbers2,cell_numbers3,cell_numbers4

     naked_list=0
     naked_values=0
     CALL Naked_pairs(cageh_real,naked_list,naked_values)
     DO inaked=1,naked_list_max
       naked_count=0
       row1=naked_list(inaked,1,1)
       col1=naked_list(inaked,1,2)
       row2=naked_list(inaked,2,1)
       col2=naked_list(inaked,2,2)
       row3=naked_list(inaked,3,1)
       col3=naked_list(inaked,3,2)
       row4=naked_list(inaked,4,1)
       col4=naked_list(inaked,4,2)
       IF (row1 .gt. 0) naked_count=naked_count+1
       IF (row2 .gt. 0) naked_count=naked_count+1
       IF (row3 .gt. 0) naked_count=naked_count+1
       IF (row4 .gt. 0) naked_count=naked_count+1
       sum_naked=0
       nvalues=0
       IF (naked_count .eq. 2) THEN
        IF ((overlap(row1,col1,1) .eq. 1) .AND. (overlap(row2,col2,1) .eq. 1)) THEN
         IF ((overlap(row1,col1,2) .eq. 0) .AND. (overlap(row2,col2,2) .eq. 0)) THEN
         ! Find the naked values from sudoku logic numbers of the two cells
           cell_numbers1=sudoku_logic_values(row1,col1)
           cell_numbers2=sudoku_logic_values(row2,col2)
           DO inumber=1,cell_numbers1
             cell_logic_number=sudoku_logic(row1,col1,inumber)
             nvalues(cell_logic_number)=1
           END DO
           DO inumber=1,cell_numbers2
             cell_logic_number=sudoku_logic(row2,col2,inumber)
             nvalues(cell_logic_number)=1
           END DO
           DO ivalue=1,9
             IF (nvalues(ivalue) .eq. 1) THEN
               sum_naked=sum_naked+ivalue
             END IF
           END DO
           sum_horizontal=sum_horizontal-sum_naked
           overlap(row1,col1,1)=0
           overlap(row2,col2,1)=0
          END IF
         END IF
        END IF
       IF (naked_count .eq. 3) THEN
        IF ((overlap(row1,col1,1) .eq. 1) .AND. (overlap(row2,col2,1) .eq. 1) &
                .AND. (overlap(row3,col3,1) .eq. 1)) THEN
         IF ((overlap(row1,col1,2) .eq. 0) .AND. (overlap(row2,col2,2) .eq. 0) &
                .AND. (overlap(row3,col3,2) .eq. 0)) THEN
         ! Find the naked values from sudoku logic numbers of the two cells
           cell_numbers1=sudoku_logic_values(row1,col1)
           cell_numbers2=sudoku_logic_values(row2,col2)
           cell_numbers3=sudoku_logic_values(row3,col3)
           DO inumber=1,cell_numbers1
             cell_logic_number=sudoku_logic(row1,col1,inumber)
             nvalues(cell_logic_number)=1
           END DO
           DO inumber=1,cell_numbers2
             cell_logic_number=sudoku_logic(row2,col2,inumber)
             nvalues(cell_logic_number)=1
           END DO
           DO inumber=1,cell_numbers3
             cell_logic_number=sudoku_logic(row3,col3,inumber)
             nvalues(cell_logic_number)=1
           END DO
           DO ivalue=1,9
             IF (nvalues(ivalue) .eq. 1) THEN
               sum_naked=sum_naked+ivalue
             END IF
           END DO
           sum_horizontal=sum_horizontal-sum_naked
           overlap(row1,col1,1)=0
           overlap(row2,col2,1)=0
           overlap(row3,col3,1)=0
          END IF
         END IF
        END IF
       IF (naked_count .eq. 4) THEN
        IF ((overlap(row1,col1,1) .eq. 1) .AND. (overlap(row2,col2,1) .eq. 1) &
                .AND. (overlap(row3,col3,1) .eq. 1) .AND. (overlap(row4,col4,1) .eq. 1)) THEN
         IF ((overlap(row1,col1,2) .eq. 0) .AND. (overlap(row2,col2,2) .eq. 0) &
                .AND. (overlap(row3,col3,2) .eq. 0) .AND. (overlap(row4,col4,2) .eq. 0)) THEN
         ! Find the naked values from sudoku logic numbers of the two cells
           cell_numbers1=sudoku_logic_values(row1,col1)
           cell_numbers2=sudoku_logic_values(row2,col2)
           cell_numbers3=sudoku_logic_values(row3,col3)
           cell_numbers4=sudoku_logic_values(row4,col4)
           DO inumber=1,cell_numbers1
             cell_logic_number=sudoku_logic(row1,col1,inumber)
             nvalues(cell_logic_number)=1
           END DO
           DO inumber=1,cell_numbers2
             cell_logic_number=sudoku_logic(row2,col2,inumber)
             nvalues(cell_logic_number)=1
           END DO
           DO inumber=1,cell_numbers3
             cell_logic_number=sudoku_logic(row3,col3,inumber)
             nvalues(cell_logic_number)=1
           END DO
           DO inumber=1,cell_numbers4
             cell_logic_number=sudoku_logic(row4,col4,inumber)
             nvalues(cell_logic_number)=1
           END DO
           DO ivalue=1,9
             IF (nvalues(ivalue) .eq. 1) THEN
               sum_naked=sum_naked+ivalue
             END IF
           END DO
           sum_horizontal=sum_horizontal-sum_naked
           overlap(row1,col1,1)=0
           overlap(row2,col2,1)=0
           overlap(row3,col3,1)=0
           overlap(row4,col4,1)=0
          END IF
         END IF
        END IF
      END DO


END SUBROUTINE naked_pairs_nonoverlap_horizontal

!------------------------------------------------------

SUBROUTINE naked_pairs_nonoverlap_vertical(cageh_real,overlap,sum_vertical)

  USE global_variables
  IMPLICIT NONE

  INTEGER :: cageh_real,sum_vertical
  INTEGER,DIMENSION(max_grid_size1,max_grid_size2,3) :: overlap
  INTEGER,DIMENSION(naked_list_max,4,2) :: naked_list
  INTEGER,DIMENSION(9) :: naked_values,nvalues
  INTEGER :: inaked,sum_naked,naked_count,ivalue
  INTEGER :: cell_logic_number,irow,icol,inumber
  INTEGER :: row1,col1,row2,col2,row3,col3,row4,col4
  INTEGER :: cell_numbers1,cell_numbers2,cell_numbers3,cell_numbers4

     naked_list=0
     naked_values=0
     CALL Naked_pairs(cageh_real,naked_list,naked_values)
     DO inaked=1,naked_list_max
       naked_count=0
       row1=naked_list(inaked,1,1)
       col1=naked_list(inaked,1,2)
       row2=naked_list(inaked,2,1)
       col2=naked_list(inaked,2,2)
       row3=naked_list(inaked,3,1)
       col3=naked_list(inaked,3,2)
       row4=naked_list(inaked,4,1)
       col4=naked_list(inaked,4,2)
       IF (row1 .gt. 0) naked_count=naked_count+1
       IF (row2 .gt. 0) naked_count=naked_count+1
       IF (row3 .gt. 0) naked_count=naked_count+1
       IF (row4 .gt. 0) naked_count=naked_count+1
       sum_naked=0
       nvalues=0
       IF (naked_count .eq. 2) THEN
        IF ((overlap(row1,col1,2) .eq. 1) .AND. (overlap(row2,col2,2) .eq. 1)) THEN
         IF ((overlap(row1,col1,1) .eq. 0) .AND. (overlap(row2,col2,1) .eq. 0)) THEN
         ! Find the naked values from sudoku logic numbers of the two cells
           cell_numbers1=sudoku_logic_values(row1,col1)
           cell_numbers2=sudoku_logic_values(row2,col2)
           DO inumber=1,cell_numbers1
             cell_logic_number=sudoku_logic(row1,col1,inumber)
             nvalues(cell_logic_number)=1
           END DO
           DO inumber=1,cell_numbers2
             cell_logic_number=sudoku_logic(row2,col2,inumber)
             nvalues(cell_logic_number)=1
           END DO
           DO ivalue=1,9
             IF (nvalues(ivalue) .eq. 1) THEN
               sum_naked=sum_naked+ivalue
             END IF
           END DO
           sum_vertical=sum_vertical-sum_naked
           overlap(row1,col1,2)=0
           overlap(row2,col2,2)=0
          END IF
         END IF
        END IF
       IF (naked_count .eq. 3) THEN
        IF ((overlap(row1,col1,2) .eq. 1) .AND. (overlap(row2,col2,2) .eq. 1) .AND. &
                (overlap(row3,col3,2) .eq. 1)) THEN
         IF ((overlap(row1,col1,1) .eq. 0) .AND. (overlap(row2,col2,1) .eq. 0) .AND. &
                (overlap(row3,col3,1) .eq. 0)) THEN
         ! Find the naked values from sudoku logic numbers of the two cells
           cell_numbers1=sudoku_logic_values(row1,col1)
           cell_numbers2=sudoku_logic_values(row2,col2)
           cell_numbers3=sudoku_logic_values(row3,col3)
           DO inumber=1,cell_numbers1
             cell_logic_number=sudoku_logic(row1,col1,inumber)
             nvalues(cell_logic_number)=1
           END DO
           DO inumber=1,cell_numbers2
             cell_logic_number=sudoku_logic(row2,col2,inumber)
             nvalues(cell_logic_number)=1
           END DO
           DO inumber=1,cell_numbers3
             cell_logic_number=sudoku_logic(row3,col3,inumber)
             nvalues(cell_logic_number)=1
           END DO
           DO ivalue=1,9
             IF (nvalues(ivalue) .eq. 1) THEN
               sum_naked=sum_naked+ivalue
             END IF
           END DO
           sum_vertical=sum_vertical-sum_naked
           overlap(row1,col1,2)=0
           overlap(row2,col2,2)=0
           overlap(row3,col3,2)=0
          END IF
         END IF
        END IF
       IF (naked_count .eq. 4) THEN
        IF ((overlap(row1,col1,2) .eq. 1) .AND. (overlap(row2,col2,2) .eq. 1) .AND. &
                (overlap(row3,col3,2) .eq. 1) .AND. (overlap(row4,col4,2) .eq. 1)) THEN
         IF ((overlap(row1,col1,1) .eq. 0) .AND. (overlap(row2,col2,1) .eq. 0) .AND. &
                (overlap(row3,col3,1) .eq. 0) .AND. (overlap(row4,col4,1) .eq. 0)) THEN
         ! Find the naked values from sudoku logic numbers of the two cells
           cell_numbers1=sudoku_logic_values(row1,col1)
           cell_numbers2=sudoku_logic_values(row2,col2)
           cell_numbers3=sudoku_logic_values(row3,col3)
           cell_numbers4=sudoku_logic_values(row4,col4)
           DO inumber=1,cell_numbers1
             cell_logic_number=sudoku_logic(row1,col1,inumber)
             nvalues(cell_logic_number)=1
           END DO
           DO inumber=1,cell_numbers2
             cell_logic_number=sudoku_logic(row2,col2,inumber)
             nvalues(cell_logic_number)=1
           END DO
           DO inumber=1,cell_numbers3
             cell_logic_number=sudoku_logic(row3,col3,inumber)
             nvalues(cell_logic_number)=1
           END DO
           DO inumber=1,cell_numbers4
             cell_logic_number=sudoku_logic(row4,col4,inumber)
             nvalues(cell_logic_number)=1
           END DO
           DO ivalue=1,9
             IF (nvalues(ivalue) .eq. 1) THEN
               sum_naked=sum_naked+ivalue
             END IF
           END DO
           sum_vertical=sum_vertical-sum_naked
           overlap(row1,col1,2)=0
           overlap(row2,col2,2)=0
           overlap(row3,col3,2)=0
           overlap(row4,col4,2)=0
          END IF
         END IF
        END IF
      END DO


END SUBROUTINE naked_pairs_nonoverlap_vertical

!-----------------------------------------------------

SUBROUTINE number_in_cells_nonoverlap(overlap,sum_horizontal,sum_vertical)

  USE global_variables
  IMPLICIT NONE

  INTEGER,DIMENSION(max_grid_size1,max_grid_size2,3) :: overlap  
  INTEGER :: sum_horizontal,sum_vertical
  INTEGER :: irow,icol

  DO irow=1,max_grid_size1
    DO icol=1,max_grid_size2
      IF (sudoku(irow,icol) .eq. 0) CYCLE
      IF (overlap(irow,icol,1) .eq. 1) THEN
        IF (overlap(irow,icol,2) .eq. 1) THEN
          CYCLE
        ELSE
          sum_horizontal = sum_horizontal-sudoku(irow,icol)
          overlap(irow,icol,1) = 0
        END IF
      ELSE
        IF (overlap(irow,icol,2) .eq. 1) THEN
          sum_vertical = sum_vertical-sudoku(irow,icol)
          overlap(irow,icol,2) = 0
        ELSE
          CYCLE
        END IF
      END IF
    END DO
  END DO

END SUBROUTINE number_in_cells_nonoverlap

!------------------------------------------------------
