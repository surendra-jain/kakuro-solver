PROGRAM Jim_solution_format

  IMPLICIT NONE

  INTEGER,PARAMETER :: max_grid_size1 = 31
  INTEGER,PARAMETER :: max_grid_size2 = 21
  INTEGER,PARAMETER :: max_cages = 10000
  INTEGER, PARAMETER :: max_cells_in_cage = 9
  INTEGER,DIMENSION(max_cages,max_cells_in_cage,2) :: cage_to_cells
  INTEGER,DIMENSION(max_cages) :: cage_sum
  INTEGER,DIMENSION(max_cages) :: cage_no_of_cells
  INTEGER :: i,j,ncells_local,cage_sum_local
  INTEGER :: ngroups_hor,ngroups_ver,ngroups_total
  INTEGER :: k,kk,jcell
  CHARACTER(LEN=max_grid_size2) :: sudoku_string

  INTEGER,DIMENSION(max_grid_size1,max_grid_size2) :: sudoku


  OPEN(UNIT=10,FILE='kakuro-initial-jim',STATUS='OLD',ACCESS='SEQUENTIAL',ACTION='READ')
  DO i=1,max_grid_size1
!      write(*,*) 'i is :',i
!     READ(10,*) sudoku_string
!     DO j=1,max_grid_size2
!	sudoku(i,j) = ICHAR(sudoku_string(j))
!     END DO 
     READ(10,*) sudoku(i,:)
     write(*,*) sudoku(i,:)
  END DO 

  ngroups_hor = 0
  ngroups_ver = 0
  ngroups_total = 0

  DO i=1,max_grid_size1
   ncells_local = 0
   cage_sum_local = 0
   DO j=1,max_grid_size2
     IF (sudoku(i,j) .eq. 0) THEN
       IF (ncells_local .eq. 0) CYCLE 
       ngroups_hor = ngroups_hor+1
       ngroups_total=ngroups_total+1
       cage_sum(ngroups_total) = cage_sum_local
       cage_no_of_cells(ngroups_total) = ncells_local
       DO k=1,ncells_local
	 kk=j-k
	 cage_to_cells(ngroups_total,k,1) = i
	 cage_to_cells(ngroups_total,k,2) = kk
       END DO
       ncells_local = 0
       cage_sum_local = 0
     ELSE
	ncells_local= ncells_local+1
	cage_sum_local = cage_sum_local+sudoku(i,j)
     END IF

     IF ((j .eq. max_grid_size2) .AND. (sudoku(i,j) .ne. 0)) THEN
       ngroups_hor = ngroups_hor+1
       ngroups_total=ngroups_total+1
       cage_sum(ngroups_total) = cage_sum_local
       cage_no_of_cells(ngroups_total) = ncells_local
       DO k=1,ncells_local
         kk=j-k+1
         cage_to_cells(ngroups_total,k,1) = i
         cage_to_cells(ngroups_total,k,2) = kk
       END DO
       ncells_local = 0
       cage_sum_local = 0
     END IF
    END DO
  END DO
		
  DO j=1,max_grid_size2
   ncells_local = 0
   cage_sum_local = 0
   DO i=1,max_grid_size1
     IF (sudoku(i,j) .eq. 0) THEN
       IF (ncells_local .eq. 0) CYCLE
       ngroups_ver = ngroups_ver+1
       ngroups_total=ngroups_total+1
       cage_sum(ngroups_total) = cage_sum_local
       cage_no_of_cells(ngroups_total) = ncells_local
       DO k=1,ncells_local
         kk=i-k
         cage_to_cells(ngroups_total,k,1) = kk
         cage_to_cells(ngroups_total,k,2) = j
       END DO
       ncells_local = 0
       cage_sum_local = 0
     ELSE
        ncells_local= ncells_local+1
        cage_sum_local = cage_sum_local+sudoku(i,j)
     END IF

     IF ((i .eq. max_grid_size1) .AND. (sudoku(i,j) .ne. 0)) THEN
       ngroups_ver = ngroups_ver+1
       ngroups_total=ngroups_total+1
       cage_sum(ngroups_total) = cage_sum_local
       cage_no_of_cells(ngroups_total) = ncells_local
       DO k=1,ncells_local
         kk=i-k+1
         cage_to_cells(ngroups_total,k,1) = kk
         cage_to_cells(ngroups_total,k,2) = j
       END DO
       ncells_local = 0
       cage_sum_local = 0
     END IF
    END DO
  END DO

! Write out the input file in our format

  OPEN(UNIT=10,FILE='kakuro-initial',STATUS='NEW',ACCESS='SEQUENTIAL')
  write(10,*) ngroups_total
  write(10,*)
  DO i=1,ngroups_total
   write(10,*) cage_sum(i),cage_no_of_cells(i)
   DO jcell=1,cage_no_of_cells(i)
     write(10,*) cage_to_cells(i,jcell,1),cage_to_cells(i,jcell,2)
   END DO
   write(10,*)
  END DO
  CLOSE(UNIT=10)  
  
 

END PROGRAM Jim_solution_format
