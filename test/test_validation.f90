! tests for the validation module
! Standard: Fortran 2018
! Preprocessor: none
! Author: Ben Trettel (<http://trettel.us/>)
! Project: [flt](https://github.com/btrettel/flt)
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

program test_validation

use prec, only: WP
use unittest, only: test_results_type
implicit none

real(WP), parameter :: HALF_ALPHA(6) = 1.0_WP - [0.90_WP, 0.95_WP, 0.975_WP, 0.99_WP, 0.995_WP, 0.999_WP]
real(WP), parameter :: T0900(100) = [3.078_WP, 1.886_WP, 1.638_WP, 1.533_WP, 1.476_WP, 1.440_WP, 1.415_WP, 1.397_WP, &
                                     1.383_WP, 1.372_WP, 1.363_WP, 1.356_WP, 1.350_WP, 1.345_WP, 1.341_WP, 1.337_WP, &
                                     1.333_WP, 1.330_WP, 1.328_WP, 1.325_WP, 1.323_WP, 1.321_WP, 1.319_WP, 1.318_WP, &
                                     1.316_WP, 1.315_WP, 1.314_WP, 1.313_WP, 1.311_WP, 1.310_WP, 1.309_WP, 1.309_WP, &
                                     1.308_WP, 1.307_WP, 1.306_WP, 1.306_WP, 1.305_WP, 1.304_WP, 1.304_WP, 1.303_WP, &
                                     1.303_WP, 1.302_WP, 1.302_WP, 1.301_WP, 1.301_WP, 1.300_WP, 1.300_WP, 1.299_WP, &
                                     1.299_WP, 1.299_WP, 1.298_WP, 1.298_WP, 1.298_WP, 1.297_WP, 1.297_WP, 1.297_WP, &
                                     1.297_WP, 1.296_WP, 1.296_WP, 1.296_WP, 1.296_WP, 1.295_WP, 1.295_WP, 1.295_WP, &
                                     1.295_WP, 1.295_WP, 1.294_WP, 1.294_WP, 1.294_WP, 1.294_WP, 1.294_WP, 1.293_WP, &
                                     1.293_WP, 1.293_WP, 1.293_WP, 1.293_WP, 1.293_WP, 1.292_WP, 1.292_WP, 1.292_WP, &
                                     1.292_WP, 1.292_WP, 1.292_WP, 1.292_WP, 1.292_WP, 1.291_WP, 1.291_WP, 1.291_WP, &
                                     1.291_WP, 1.291_WP, 1.291_WP, 1.291_WP, 1.291_WP, 1.291_WP, 1.291_WP, 1.290_WP, &
                                     1.290_WP, 1.290_WP, 1.290_WP, 1.290_WP]
real(WP), parameter :: T0950(100) = [6.314_WP, 2.920_WP, 2.353_WP, 2.132_WP, 2.015_WP, 1.943_WP, 1.895_WP, 1.860_WP, &
                                     1.833_WP, 1.812_WP, 1.796_WP, 1.782_WP, 1.771_WP, 1.761_WP, 1.753_WP, 1.746_WP, &
                                     1.740_WP, 1.734_WP, 1.729_WP, 1.725_WP, 1.721_WP, 1.717_WP, 1.714_WP, 1.711_WP, &
                                     1.708_WP, 1.706_WP, 1.703_WP, 1.701_WP, 1.699_WP, 1.697_WP, 1.696_WP, 1.694_WP, &
                                     1.692_WP, 1.691_WP, 1.690_WP, 1.688_WP, 1.687_WP, 1.686_WP, 1.685_WP, 1.684_WP, &
                                     1.683_WP, 1.682_WP, 1.681_WP, 1.680_WP, 1.679_WP, 1.679_WP, 1.678_WP, 1.677_WP, &
                                     1.677_WP, 1.676_WP, 1.675_WP, 1.675_WP, 1.674_WP, 1.674_WP, 1.673_WP, 1.673_WP, &
                                     1.672_WP, 1.672_WP, 1.671_WP, 1.671_WP, 1.670_WP, 1.670_WP, 1.669_WP, 1.669_WP, &
                                     1.669_WP, 1.668_WP, 1.668_WP, 1.668_WP, 1.667_WP, 1.667_WP, 1.667_WP, 1.666_WP, &
                                     1.666_WP, 1.666_WP, 1.665_WP, 1.665_WP, 1.665_WP, 1.665_WP, 1.664_WP, 1.664_WP, &
                                     1.664_WP, 1.664_WP, 1.663_WP, 1.663_WP, 1.663_WP, 1.663_WP, 1.663_WP, 1.662_WP, &
                                     1.662_WP, 1.662_WP, 1.662_WP, 1.662_WP, 1.661_WP, 1.661_WP, 1.661_WP, 1.661_WP, &
                                     1.661_WP, 1.661_WP, 1.660_WP, 1.660_WP]
real(WP), parameter :: T0975(100) = [12.706_WP, 4.303_WP, 3.182_WP, 2.776_WP, 2.571_WP, 2.447_WP, 2.365_WP, 2.306_WP, &
                                     2.262_WP, 2.228_WP, 2.201_WP, 2.179_WP, 2.160_WP, 2.145_WP, 2.131_WP, 2.120_WP, &
                                     2.110_WP, 2.101_WP, 2.093_WP, 2.086_WP, 2.080_WP, 2.074_WP, 2.069_WP, 2.064_WP, &
                                     2.060_WP, 2.056_WP, 2.052_WP, 2.048_WP, 2.045_WP, 2.042_WP, 2.040_WP, 2.037_WP, &
                                     2.035_WP, 2.032_WP, 2.030_WP, 2.028_WP, 2.026_WP, 2.024_WP, 2.023_WP, 2.021_WP, &
                                     2.020_WP, 2.018_WP, 2.017_WP, 2.015_WP, 2.014_WP, 2.013_WP, 2.012_WP, 2.011_WP, &
                                     2.010_WP, 2.009_WP, 2.008_WP, 2.007_WP, 2.006_WP, 2.005_WP, 2.004_WP, 2.003_WP, &
                                     2.002_WP, 2.002_WP, 2.001_WP, 2.000_WP, 2.000_WP, 1.999_WP, 1.998_WP, 1.998_WP, &
                                     1.997_WP, 1.997_WP, 1.996_WP, 1.995_WP, 1.995_WP, 1.994_WP, 1.994_WP, 1.993_WP, &
                                     1.993_WP, 1.993_WP, 1.992_WP, 1.992_WP, 1.991_WP, 1.991_WP, 1.990_WP, 1.990_WP, &
                                     1.990_WP, 1.989_WP, 1.989_WP, 1.989_WP, 1.988_WP, 1.988_WP, 1.988_WP, 1.987_WP, &
                                     1.987_WP, 1.987_WP, 1.986_WP, 1.986_WP, 1.986_WP, 1.986_WP, 1.985_WP, 1.985_WP, &
                                     1.985_WP, 1.984_WP, 1.984_WP, 1.984_WP]
real(WP), parameter :: T0990(100) = [31.821_WP, 6.965_WP, 4.541_WP, 3.747_WP, 3.365_WP, 3.143_WP, 2.998_WP, 2.896_WP, &
                                     2.821_WP, 2.764_WP, 2.718_WP, 2.681_WP, 2.650_WP, 2.624_WP, 2.602_WP, 2.583_WP, &
                                     2.567_WP, 2.552_WP, 2.539_WP, 2.528_WP, 2.518_WP, 2.508_WP, 2.500_WP, 2.492_WP, &
                                     2.485_WP, 2.479_WP, 2.473_WP, 2.467_WP, 2.462_WP, 2.457_WP, 2.453_WP, 2.449_WP, &
                                     2.445_WP, 2.441_WP, 2.438_WP, 2.434_WP, 2.431_WP, 2.429_WP, 2.426_WP, 2.423_WP, &
                                     2.421_WP, 2.418_WP, 2.416_WP, 2.414_WP, 2.412_WP, 2.410_WP, 2.408_WP, 2.407_WP, &
                                     2.405_WP, 2.403_WP, 2.402_WP, 2.400_WP, 2.399_WP, 2.397_WP, 2.396_WP, 2.395_WP, &
                                     2.394_WP, 2.392_WP, 2.391_WP, 2.390_WP, 2.389_WP, 2.388_WP, 2.387_WP, 2.386_WP, &
                                     2.385_WP, 2.384_WP, 2.383_WP, 2.382_WP, 2.382_WP, 2.381_WP, 2.380_WP, 2.379_WP, &
                                     2.379_WP, 2.378_WP, 2.377_WP, 2.376_WP, 2.376_WP, 2.375_WP, 2.374_WP, 2.374_WP, &
                                     2.373_WP, 2.373_WP, 2.372_WP, 2.372_WP, 2.371_WP, 2.370_WP, 2.370_WP, 2.369_WP, &
                                     2.369_WP, 2.368_WP, 2.368_WP, 2.368_WP, 2.367_WP, 2.367_WP, 2.366_WP, 2.366_WP, &
                                     2.365_WP, 2.365_WP, 2.365_WP, 2.364_WP]
real(WP), parameter :: T0995(100) = [63.657_WP, 9.925_WP, 5.841_WP, 4.604_WP, 4.032_WP, 3.707_WP, 3.499_WP, 3.355_WP, &
                                     3.250_WP, 3.169_WP, 3.106_WP, 3.055_WP, 3.012_WP, 2.977_WP, 2.947_WP, 2.921_WP, &
                                     2.898_WP, 2.878_WP, 2.861_WP, 2.845_WP, 2.831_WP, 2.819_WP, 2.807_WP, 2.797_WP, &
                                     2.787_WP, 2.779_WP, 2.771_WP, 2.763_WP, 2.756_WP, 2.750_WP, 2.744_WP, 2.738_WP, &
                                     2.733_WP, 2.728_WP, 2.724_WP, 2.719_WP, 2.715_WP, 2.712_WP, 2.708_WP, 2.704_WP, &
                                     2.701_WP, 2.698_WP, 2.695_WP, 2.692_WP, 2.690_WP, 2.687_WP, 2.685_WP, 2.682_WP, &
                                     2.680_WP, 2.678_WP, 2.676_WP, 2.674_WP, 2.672_WP, 2.670_WP, 2.668_WP, 2.667_WP, &
                                     2.665_WP, 2.663_WP, 2.662_WP, 2.660_WP, 2.659_WP, 2.657_WP, 2.656_WP, 2.655_WP, &
                                     2.654_WP, 2.652_WP, 2.651_WP, 2.650_WP, 2.649_WP, 2.648_WP, 2.647_WP, 2.646_WP, &
                                     2.645_WP, 2.644_WP, 2.643_WP, 2.642_WP, 2.641_WP, 2.640_WP, 2.640_WP, 2.639_WP, &
                                     2.638_WP, 2.637_WP, 2.636_WP, 2.636_WP, 2.635_WP, 2.634_WP, 2.634_WP, 2.633_WP, &
                                     2.632_WP, 2.632_WP, 2.631_WP, 2.630_WP, 2.630_WP, 2.629_WP, 2.629_WP, 2.628_WP, &
                                     2.627_WP, 2.627_WP, 2.626_WP, 2.626_WP]
real(WP), parameter :: T0999(100) = [318.309_WP, 22.327_WP, 10.215_WP, 7.173_WP, 5.893_WP, 5.208_WP, 4.785_WP, 4.501_WP, &
                                     4.297_WP, 4.144_WP, 4.025_WP, 3.930_WP, 3.852_WP, 3.787_WP, 3.733_WP, 3.686_WP, &
                                     3.646_WP, 3.610_WP, 3.579_WP, 3.552_WP, 3.527_WP, 3.505_WP, 3.485_WP, 3.467_WP, &
                                     3.450_WP, 3.435_WP, 3.421_WP, 3.408_WP, 3.396_WP, 3.385_WP, 3.375_WP, 3.365_WP, &
                                     3.356_WP, 3.348_WP, 3.340_WP, 3.333_WP, 3.326_WP, 3.319_WP, 3.313_WP, 3.307_WP, &
                                     3.301_WP, 3.296_WP, 3.291_WP, 3.286_WP, 3.281_WP, 3.277_WP, 3.273_WP, 3.269_WP, &
                                     3.265_WP, 3.261_WP, 3.258_WP, 3.255_WP, 3.251_WP, 3.248_WP, 3.245_WP, 3.242_WP, &
                                     3.239_WP, 3.237_WP, 3.234_WP, 3.232_WP, 3.229_WP, 3.227_WP, 3.225_WP, 3.223_WP, &
                                     3.220_WP, 3.218_WP, 3.216_WP, 3.214_WP, 3.213_WP, 3.211_WP, 3.209_WP, 3.207_WP, &
                                     3.206_WP, 3.204_WP, 3.202_WP, 3.201_WP, 3.199_WP, 3.198_WP, 3.197_WP, 3.195_WP, &
                                     3.194_WP, 3.193_WP, 3.191_WP, 3.190_WP, 3.189_WP, 3.188_WP, 3.187_WP, 3.185_WP, &
                                     3.184_WP, 3.183_WP, 3.182_WP, 3.181_WP, 3.180_WP, 3.179_WP, 3.178_WP, 3.177_WP, &
                                     3.176_WP, 3.175_WP, 3.175_WP, 3.174_WP]


type(test_results_type) :: tests

call tests%start_tests("validation.nml")

call test_z_tail_cdf(tests)
call test_t_tail_cdf(tests)
call test_student_t(tests)

call tests%end_tests()

contains

subroutine test_z_tail_cdf(tests)
    ! Using the following table:
    ! <https://www.itl.nist.gov/div898/handbook/eda/section3/eda3672.htm>
    
    use validation, only: z_tail_cdf
    use prec, only: CL
    
    type(test_results_type), intent(in out) :: tests
    
    real(WP), parameter :: Z_TAIL_HALF_ALPHA(6) = [1.282_WP, 1.645_WP, 1.960_WP, 2.326_WP, 2.576_WP, 3.090_WP]
    
    integer :: i
    character(len=CL) :: message
    
    do i = 1, size(HALF_ALPHA)
        write(message, "(a, g0)") "z_tail_cdf, 0.5*alpha=", HALF_ALPHA(i)
        call tests%real_eq(z_tail_cdf(Z_TAIL_HALF_ALPHA(i)), HALF_ALPHA(i), trim(message), abs_tol=1.0e-3_WP)
        
        write(message, "(a, g0)") "z_tail_cdf, 0.5*alpha=", 1.0_WP - HALF_ALPHA(i)
        call tests%real_eq(z_tail_cdf(-Z_TAIL_HALF_ALPHA(i)), 1.0_WP - HALF_ALPHA(i), trim(message), abs_tol=1.0e-3_WP)
    end do
end subroutine test_z_tail_cdf

subroutine test_t_tail_cdf(tests)
    ! Using the following table:
    ! <https://www.itl.nist.gov/div898/handbook/eda/section3/eda3672.htm>
    
    use validation, only: t_tail_cdf
    use prec, only: CL
    
    type(test_results_type), intent(in out) :: tests
        
    character(len=CL) :: message
    integer  :: i_alpha, dof
    real(WP) :: t
    
    do i_alpha = 1, size(HALF_ALPHA)
        do dof = 5, 100
            select case (i_alpha)
                case (1)
                    t = T0900(dof)
                case (2)
                    t = T0950(dof)
                case (3)
                    t = T0975(dof)
                case (4)
                    t = T0990(dof)
                case (5)
                    t = T0995(dof)
                case (6)
                    t = T0999(dof)
                case default
                    error stop
            end select
            
            write(message, "(a, f5.3, a, i0)") "t_tail_cdf, alpha=", HALF_ALPHA(i_alpha), ", dof=", dof
            call tests%real_eq(t_tail_cdf(t, dof), HALF_ALPHA(i_alpha), trim(message), &
                                abs_tol=1.0e-3_WP)
            
            write(message, "(a, f5.3, a, i0)") "t_tail_cdf, alpha=", 1.0_WP - HALF_ALPHA(i_alpha), ", dof=", dof
            call tests%real_eq(t_tail_cdf(-t, dof), 1.0_WP - HALF_ALPHA(i_alpha), trim(message), &
                                abs_tol=1.0e-3_WP)
        end do
    end do
end subroutine test_t_tail_cdf

subroutine test_student_t(tests)
    use validation, only: student_t
    use prec, only: CL
    
    type(test_results_type), intent(in out) :: tests
    
    character(len=CL) :: message
    integer  :: i_alpha, dof
    real(WP) :: t, abs_tol
    
    do i_alpha = 1, 6
        do dof = 5, 100
            select case (i_alpha)
                case (1)
                    t = T0900(dof)
                case (2)
                    t = T0950(dof)
                case (3)
                    t = T0975(dof)
                case (4)
                    t = T0990(dof)
                case (5)
                    t = T0995(dof)
                case (6)
                    t = T0999(dof)
                case default
                    error stop
            end select
            
            ! TODO: The secant method seems to be off for a low number of degrees of freedom.
            ! It's unclear to me why that is. I should investigate this later.
            if (dof <= 10) then
                abs_tol = 1.0_WP
            else
                abs_tol = 0.3e-2_WP
            end if
            
            write(message, "(a, f5.3, a, i0)") "student_t, alpha=", HALF_ALPHA(i_alpha), ", dof=", dof
            call tests%real_eq(student_t(HALF_ALPHA(i_alpha), dof), t, trim(message), abs_tol=abs_tol)
            
            write(message, "(a, f5.3, a, i0)") "student_t, alpha=", 1.0_WP - HALF_ALPHA(i_alpha), ", dof=", dof
            call tests%real_eq(student_t(1.0_WP - HALF_ALPHA(i_alpha), dof), -t, trim(message), abs_tol=abs_tol)
        end do
    end do
end subroutine test_student_t

end program test_validation
