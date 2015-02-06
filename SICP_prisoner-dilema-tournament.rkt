#lang racket
#|
;; patsy
            PATSY     NASTY    SPASTIC    TOUGH     SOFT     COMBINED

PATSY    |    4    |    2    |    3    |    4    |    4    |    4    |
         |---------|---------|---------|---------|---------|---------|
NASTY    |         |    0    |    1    |    0    |    2    |    1    |
         |---------|---------|---------|---------|---------|---------|
SPASTIC  |         |         |   1.8   |    2    |   3.1   |    2.5  |
         |---------|---------|---------|---------|---------|---------|
TOUGH    |         |         |         |    4    |    4    |    4    |
         |---------|---------|---------|---------|---------|---------|
SOFT     |         |         |         |         |    4    |    4    |
         |---------|---------|---------|---------|---------|---------|
COMBINED |         |         |         |         |         |    4    |

  Patsy does best agains any combination of patsy, soft, tough, or combined
  It does poorly whenever nasty is involved, but soft eye-for-eye mitigates the effect somehow
  Genereally patsy works better whenever other players cooperate more than defect

;; nasty

            PATSY     NASTY    SPASTIC    TOUGH     SOFT     COMBINED

PATSY    |         |         |         |         |         |         |
         |         |         |         |         |         |         |
NASTY    |         |         |         |         |         |         |
         |         |         |         |         |         |         |
SPASTIC  |         |         |         |         |         |         |
         |         |         |         |         |         |         |
TOUGH    |         |         |         |         |         |         |
         |         |         |         |         |         |         |
SOFT     |         |         |         |         |         |         |
         |         |         |         |         |         |         |
COMBINED |         |         |         |         |         |         |


;; spastic

            PATSY     NASTY    SPASTIC    TOUGH     SOFT     COMBINED

PATSY    |         |         |         |         |         |         |
         |         |         |         |         |         |         |
NASTY    |         |         |         |         |         |         |
         |         |         |         |         |         |         |
SPASTIC  |         |         |         |         |         |         |
         |         |         |         |         |         |         |
TOUGH    |         |         |         |         |         |         |
         |         |         |         |         |         |         |
SOFT     |         |         |         |         |         |         |
         |         |         |         |         |         |         |
COMBINED |         |         |         |         |         |         |


;; tough

            PATSY     NASTY    SPASTIC    TOUGH     SOFT     COMBINED

PATSY    |         |         |         |         |         |         |
         |         |         |         |         |         |         |
NASTY    |         |         |         |         |         |         |
         |         |         |         |         |         |         |
SPASTIC  |         |         |         |         |         |         |
         |         |         |         |         |         |         |
TOUGH    |         |         |         |         |         |         |
         |         |         |         |         |         |         |
SOFT     |         |         |         |         |         |         |
         |         |         |         |         |         |         |
COMBINED |         |         |         |         |         |         |


;; soft

            PATSY     NASTY    SPASTIC    TOUGH     SOFT     COMBINED

PATSY    |         |         |         |         |         |         |
         |         |         |         |         |         |         |
NASTY    |         |         |         |         |         |         |
         |         |         |         |         |         |         |
SPASTIC  |         |         |         |         |         |         |
         |         |         |         |         |         |         |
TOUGH    |         |         |         |         |         |         |
         |         |         |         |         |         |         |
SOFT     |         |         |         |         |         |         |
         |         |         |         |         |         |         |
COMBINED |         |         |         |         |         |         |


;; combined

            PATSY     NASTY    SPASTIC    TOUGH     SOFT     COMBINED

PATSY    |         |         |         |         |         |         |
         |         |         |         |         |         |         |
NASTY    |         |         |         |         |         |         |
         |         |         |         |         |         |         |
SPASTIC  |         |         |         |         |         |         |
         |         |         |         |         |         |         |
TOUGH    |         |         |         |         |         |         |
         |         |         |         |         |         |         |
SOFT     |         |         |         |         |         |         |
         |         |         |         |         |         |         |
COMBINED |         |         |         |         |         |         |
|#