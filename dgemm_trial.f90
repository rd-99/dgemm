Module m
    implicit none
    contains
    subroutine multiply(X,Y,Q)
 
        real*8, dimension(:,:,:,:) , intent(in) :: X , Y
        real*8, dimension(:,:,:,:) ,allocatable,intent(out) :: Q

        integer x1,x2,x3,x4,y1,y2,y3,y4,I,J,L,T

        x1 = SIZE(X(:,1,1,1))
        x2 = SIZE(X(1,:,1,1))
        x3 = SIZE(X(1,1,:,1))
        x4 = SIZE(X(1,1,1,:))
        y1 = SIZE(Y(:,1,1,1))
        y2 = SIZE(Y(1,:,1,1))
        y3 = SIZE(Y(1,1,:,1))
        y4 = SIZE(Y(1,1,1,:))    
        print *, x1,x2,y1,y2

        IF ( ( x3 ==  y1) .AND. ( x4 == y2 ) ) THEN
            allocate (Q(x1,x2,y3,y4))
            CALL DGEMM('N','N',x1*x2,y3*y4,x3*x4,2,X,x1*x2,Y,y1*y2,1,Q,x1*x2)        
            PRINT *, "First case"
            DO I = 1, 3
                DO J = 1, 2
                    DO L = 1,2
                        DO T = 1,2
                            write(*,*) Q(I,J,L,T) + 77
                        END DO
                    END DO
                END DO
            END DO



                       
      ELSE IF (( x2 ==  y1 ) .AND. (x1 ==  y2)) THEN
            allocate (Q(1:x4,1:x3,1:y3,1:y4))
            CALL DGEMM('T','N',x4*x3,y3*y4,x1*x2,1,X,x3*x4,Y,y1*y2,0,Q,x3*x4)        
            PRINT *, "2nd case"

      ELSE IF (( x3 ==  y4) .AND. ( x4 ==  y3) ) THEN
            allocate (Q(1:x1,1:x2,1:y2,1:y1))
            CALL DGEMM('N','T',x1*x2,y1*y2,x3*x4,1,X,x1*x2,Y,y3*y4,0,Q,x1*x2)        
            PRINT *, "3rd case"

      ELSE IF ((x3 ==  y1) .AND. (y4 ==  y2) ) THEN
            allocate (Q(x4,x3,y2,y1))
            CALL DGEMM('N','N',x1*x2,y3*y4,x3*x4,1,X,x1*x2,Y,y1*y2,0,Q,x1*x2)        
            PRINT *, "4rd case"

      END IF

      return
    end subroutine multiply 
    end module m

    program test
    use m
    implicit none

    real*8,dimension(5,5,10,10) :: X
    real*8,dimension(10,10,10,10) :: Y

    real*8,dimension(:,:,:,:) , allocatable :: Q
    integer I,J,L,T

    DO I = 1, 5
        DO J = 1, 5
            DO L = 1,10
                DO T = 1,10
                    X(I,J,L,T) = 1.d0
                END DO
            END DO
        END DO
      END DO


      DO I = 1, 10
        DO J = 1,10
            DO L = 1,10
                DO T = 1,10
                    Y(I,J,L,T) = 1.d0
                END DO
            END DO
        END DO
      END DO



      call multiply(X,Y,Q)

      DO I = 1, 3
        DO J = 1, 2
            DO L = 1,2
                DO T = 1,2
                    write(*,*) Q(I,J,L,T) + 1
                END DO
            END DO
        END DO
      END DO

      stop 
      end program test



