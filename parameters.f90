MODULE parameters

USE constants
IMPLICIT NONE

   INTEGER, PARAMETER:: NO_DEBYE_LENGTHS = 800
   INTEGER, PARAMETER:: NO_CELLS_PER_DEBYE = 4
   INTEGER, PARAMETER:: NO_ION_SPECIES = 1
  

   REAL(KIND = DOUBLE), PARAMETER:: TEMPERATURE = 1.5D0
   REAL(KIND = DOUBLE), PARAMETER:: E_PARTICLE_DENSITY = 1.0e18
   REAL(KIND = DOUBLE), PARAMETER:: ELECTRON_CHARGE2 = ELECTRON_CHARGE &
                                                       *ELECTRON_CHARGE

   REAL(KIND = DOUBLE), PARAMETER:: NEUTRAL_DENSITY = 1.0e18
   REAL(KIND = DOUBLE), PARAMETER:: MACROPARTICLE_SIZE = 3.0e10
   REAL(KIND = DOUBLE), PARAMETER:: ION_MACROPARTICLE_SIZE = 3.0e10
   REAL(KIND = DOUBLE), PARAMETER:: ARGON_MASS = 39.948D0
   REAL(KIND = DOUBLE), PARAMETER:: ION_DENSITY = 1.0e18
  

   CONTAINS

     FUNCTION return_debye_L()
        IMPLICIT NONE
        REAL(KIND = DOUBLE):: return_debye_L
        return_debye_L = DSQRT(PERMITTIVITY_FREE_SPACE*TEMPERATURE/ &
                            (E_PARTICLE_DENSITY*DABS(ELECTRON_CHARGE))) 
     END FUNCTION return_debye_L 

     FUNCTION return_length()
         IMPLICIT NONE
         REAL(KIND = DOUBLE):: return_length
         return_length = DBLE(NO_DEBYE_LENGTHS)*return_debye_L()
     END FUNCTION return_length 

     FUNCTION return_no_cells()
         IMPLICIT NONE
         INTEGER:: return_no_cells
         return_no_cells = NO_DEBYE_LENGTHS*NO_CELLS_PER_DEBYE
     END FUNCTION return_no_cells

     FUNCTION return_no_particles()
         IMPLICIT NONE
         INTEGER(KIND = LONG_INT):: return_no_particles
         return_no_particles = LONG(E_PARTICLE_DENSITY/MACROPARTICLE_SIZE &
                                      *return_length())
     END FUNCTION return_no_particles	

    FUNCTION return_ion_mass()
        IMPLICIT NONE
        REAL(KIND = DOUBLE):: return_ion_mass
        return_ion_mass = ARGON_MASS*AMU
    END FUNCTION return_ion_mass

    FUNCTION return_ion_charge()
        IMPLICIT NONE
        REAL(KIND = DOUBLE):: return_ion_charge
        return_ion_charge = DABS(ELECTRON_CHARGE)
    END FUNCTION return_ion_charge

    FUNCTION return_plasma_freq()
        IMPLICIT NONE
        REAL(KIND = DOUBLE):: return_plasma_freq
        return_plasma_freq = DSQRT(E_PARTICLE_DENSITY*ELECTRON_CHARGE2/ &
                             (PERMITTIVITY_FREE_SPACE*ELECTRON_MASS))
    END FUNCTION return_plasma_freq
        
    FUNCTION return_dx()
        IMPLICIT NONE
        REAL(KIND = DOUBLE):: return_dx
        return_dx = return_length()/DBLE(return_no_cells())
    END FUNCTION return_dx
   
    FUNCTION return_qo()
        IMPLICIT NONE
        REAL(KIND = DOUBLE):: return_qo
        return_qo = E_PARTICLE_DENSITY*DABS(ELECTRON_CHARGE)*return_dx()
    END FUNCTION return_qo

    FUNCTION return_qce()
       IMPLICIT NONE
       REAL(KIND = DOUBLE)::return_qce 
       return_qce= MACROPARTICLE_SIZE*ELECTRON_CHARGE
    END FUNCTION return_qce
      
    FUNCTION return_qci()
       IMPLICIT NONE
       REAL(KIND = DOUBLE)::return_qci
       return_qci= ION_MACROPARTICLE_SIZE*return_ion_charge()
    END FUNCTION return_qci
      
    FUNCTION return_dt()
        IMPLICIT NONE
        REAL(KIND = DOUBLE):: return_dt
        return_dt = 0.01D0/return_plasma_freq()
    END FUNCTION return_dt

    FUNCTION return_dxpe()
        IMPLICIT NONE
        REAL(KIND = DOUBLE):: return_dxpe,length
        length = return_length() 
        return_dxpe = length/(DBLE(return_no_particles() + 1))
    END FUNCTION return_dxpe 
        
    FUNCTION return_dxpi()
        IMPLICIT NONE
        REAL(KIND = DOUBLE):: return_dxpi,length
        length = return_length() 
        return_dxpi = length/(DBLE(return_no_particles() + 1))
    END FUNCTION return_dxpi 

    FUNCTION return_vte()
       IMPLICIT NONE
       REAL(KIND = DOUBLE):: return_vte
       return_vte = DSQRT((DABS(ELECTRON_CHARGE))*TEMPERATURE/ &
                          ELECTRON_MASS)
    END FUNCTION return_vte 

        
    FUNCTION return_vti()
       IMPLICIT NONE
       REAL(KIND = DOUBLE):: return_vti
       return_vti = DSQRT((DABS(ELECTRON_CHARGE))*TEMPERATURE/ &
                          return_ion_mass())
    END FUNCTION return_vti

    FUNCTION return_vtn()
       IMPLICIT NONE
       REAL(KIND = DOUBLE):: return_vtn
       return_vtn = DSQRT((DABS(ELECTRON_CHARGE))*0.025/ &
                          return_ion_mass())
    END FUNCTION return_vtn

END MODULE parameters
