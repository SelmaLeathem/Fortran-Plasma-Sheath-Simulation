MODULE constants
   IMPLICIT NONE
   
   INTEGER, PARAMETER:: DOUBLE = KIND(1D0)
   INTEGER, PARAMETER:: LONG_INT = selected_int_kind(8)

   INTEGER, PARAMETER:: UPPER_LIMIT = 10000
   INTEGER, PARAMETER:: SEED = 1
   REAL(KIND = DOUBLE), PARAMETER :: PI = 3.141592654D0
   

   REAL(KIND = DOUBLE), PARAMETER :: ELECTRON_CHARGE = -1.602177e-19
   REAL(KIND = DOUBLE), PARAMETER :: ELECTRON_MASS = 9.10939e-31
   REAL(KIND = DOUBLE), PARAMETER :: AMU = 1.66054e-27

   REAL(KIND = DOUBLE), PARAMETER :: BOLTZMANN_CONSTANT = 1.38066e-23
   REAL(KIND = DOUBLE), PARAMETER :: PERMITTIVITY_FREE_SPACE= 8.854e-12
   REAL(KIND = DOUBLE), PARAMETER :: PERMEABILITY_FREE_SPACE= 4.d0*PI*1.0e-7
END MODULE constants

