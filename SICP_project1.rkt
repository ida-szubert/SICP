#lang racket
;;; Project 1, 6.001, Spring 2005

;;; idea is to simulate a baseball robot

;; imagine hitting a ball with an initial velocity of v 
;; at an angle alpha from the horizontal, at a height h
;; we would like to know how far the ball travels.

;; as a first step, we can just model this with simple physics
;; so the equations of motion for the ball have a vertical and a 
;; horizontal component

;; the vertical component is governed by
;; y(t) = v sin alpha t + h - 1/2 g t^2 
;; where g is the gravitational constant of 9.8 m/s^2

;; the horizontal component is governed by
;; x(t) = v cos alpha t
;; assuming it starts at the origin

;; First, we want to know when the ball hits the ground
;; this is governed by the quadratic equation, so we just need to know when 
;; y(t)=0 (i.e. for what t_impact is y(t_impact)= 0).
;; note that there are two solutions, only one makes sense physically

(define square
  (lambda (x) (* x x)))

;; these are constants that will be useful to us
(define gravity 9.8)  ;; in m/s
(define pi 3.14159)

;; Problem 1

(define position
  (lambda (a v u t)
    (+ (* (* a 0.5) (square t))
       (* v t)
       u)))

;; you need to complete this procedure, then show some test cases

; (position 0 0 0 0) -> 0
; (position 0 0 20 0) -> 20
; (position 0 5 10 10) -> 60
; (position 2 2 2 2) -> 10
; (position 5 5 5 5) -> 92.5


;; Problem 2

(define root1
  (lambda (a b c)
    (let ((f (lambda (x) (+ (* a (square x)) (* b x) c)))
          (vertex (/ (- b) (* 2 a))))      
      (if (or (and (positive? a) (positive? (f vertex)))
              (and (negative? a) (negative? (f vertex))))
          #f ;means there are no zeros
          (/ (- 0 (+ b (sqrt (- (square b) (* 4 a c)))))
             (* 2 a))))))
    
(define root2
  (lambda (a b c)
       (let ((f (lambda (x) (+ (* a (square x)) (* b x) c)))
          (vertex (/ (- b) (* 2 a))))      
      (if (or (and (positive? a) (positive? (f vertex)))
              (and (negative? a) (negative? (f vertex))))
          #f ;means there are no zeros
          (/ (- 0 (- b (sqrt (- (square b) (* 4 a c)))))
             (* 2 a))))))
(root1 5 3 6)
(root2 5 3 6)

;; complete these procedures and show some test cases

;; Problem 3

(define time-to-impact
  (lambda (vertical-velocity elevation)
    (root1 (- (* gravity 0.5)) vertical-velocity elevation)))

;; Note that if we want to know when the ball drops to a particular height r 
;; (for receiver), we have

(define time-to-height
  (lambda (vertical-velocity elevation target-elevation)
    (root1 (- (* gravity 0.5)) vertical-velocity (- elevation target-elevation))))
;the idea is that we're calculating the zero point of the same function
;only shifted downwards - so that the interception points at y=target-elevation
;are not interception points at y=0

(time-to-impact 20 1.8)
(time-to-height 20 1.8 5)
(time-to-impact 0 1.8) ;droping a ball from head-hight
(time-to-height 20 1.8 30) ;should not reach 30m at all

;; Problem 4

;; once we can solve for t_impact, we can use it to figure out how far the ball went

;; conversion procedure
(define degree2radian
  (lambda (deg)
    (/ (* deg pi) 180.)))

(define travel-distance-simple
  (lambda (elevation velocity angle)
    (let ((radian-angle (degree2radian angle)))
      (let ((vertical-velocity (* (sin radian-angle) velocity))
            (horizontal-velocity (* (cos radian-angle) velocity)))
        (* horizontal-velocity (time-to-impact vertical-velocity elevation))))))

(* (sin 0) 45)
(* (cos 0) 45)

;; let's try this out for some example values.  Note that we are going to 
;; do everything in metric units, but for quaint reasons it is easier to think
;; about things in English units, so we will need some conversions.

(define meters-to-feet
  (lambda (m)
    (/ (* m 39.6) 12)))

(define feet-to-meters
  (lambda (f)
    (/ (* f 12) 39.6)))

(define hours-to-seconds
  (lambda (h)
    (* h 3600)))

(define seconds-to-hours
  (lambda (s)
    (/ s 3600)))

;; what is time to impact for a ball hit at a height of 1 meter
;; with a velocity of 45 m/s (which is about 100 miles/hour)
;; at an angle of 0 (straight horizontal)
;; at an angle of (/ pi 2) radians or 90 degrees (straight vertical)
;; at an angle of (/ pi 4) radians or 45 degrees

;; what is the distance traveled in each case?
;; record both in meters and in feet

(travel-distance-simple 1 45 0)
;-> 20.3
(travel-distance-simple 1 45 90)
;-> 0
(travel-distance-simple 1 45 45)
;-> 207.6
;seems strange, but when angle is 0, there's no vertical velocity to counteract gravity
;and the ball falls to the ground fast, not having time to travel a long distance
;I don't get imperial units, so I'm passing on this part


;; Problem 5

;; these sound pretty impressive, but we need to look at it more carefully

;; first, though, suppose we want to find the angle that gives the best
;; distance
;; assume that angle is between 0 and (/ pi 2) radians or between 0 and 90
;; degrees

(define alpha-increment 1)

(define find-best-angle
  (lambda (velocity elevation)
    (define (try new-guess last-guess limit)
      (let ((last-distance (travel-distance-simple elevation velocity last-guess))
            (new-distance (travel-distance-simple elevation velocity new-guess)))
        (if (> new-guess limit)
          last-guess
          (if (> last-distance new-distance)
              last-guess
              (try (+ new-guess alpha-increment) new-guess limit)))))
    (try 1 0 90)))

;; find best angle
(find-best-angle 45 1)
; -> 45
;; try for other velocities
(find-best-angle 40 1)
(find-best-angle 39 1)
(find-best-angle 38 1)
(find-best-angle 20 1)
(find-best-angle 15 1)
;; try for other heights
(find-best-angle 45 0.8)
(find-best-angle 45 0.9)
(find-best-angle 45 1.1)
(find-best-angle 45 1.2)
(find-best-angle 45 1.3)
(find-best-angle 45 1.4)

;no matter the velocity or elevation, the best angle is 45 degrees

;; Problem 6

;; problem is that we are not accounting for drag on the ball (or on spin 
;; or other effects, but let's just stick with drag)
;;
;; Newton's equations basically say that ma = F, and here F is really two 
;; forces.  One is the effect of gravity, which is captured by mg.  The
;; second is due to drag, so we really have
;;
;; a = drag/m + gravity
;;
;; drag is captured by 1/2 C rho A vel^2, where
;; C is the drag coefficient (which is about 0.5 for baseball sized spheres)
;; rho is the density of air (which is about 1.25 kg/m^3 at sea level 
;; with moderate humidity, but is about 1.06 in Denver)
;; A is the surface area of the cross section of object, which is pi D^2/4 
;; where D is the diameter of the ball (which is about 0.074m for a baseball)
;; thus drag varies by the square of the velocity, with a scaling factor 
;; that can be computed

;; We would like to again compute distance , but taking into account 
;; drag.
;; Basically we can rework the equations to get four coupled linear 
;; differential equations
;; let u be the x component of velocity, and v be the y component of velocity
;; let x and y denote the two components of position (we are ignoring the 
;; third dimension and are assuming no spin so that a ball travels in a plane)
;; the equations are
;;
;; dx/dt = u
;; dy/dt = v
;; du/dt = -(drag_x/m + g_x)
;; dv/dt = -(drag_y/m + g_y)
;; we have g_x = - and g_y = - gravity
;; to get the components of the drag force, we need some trig.
;; let speeed = (u^2+v^2)^(1/2), then
;; drag_x = - drag * u /speed
;; drag_y = - drag * v /speed
;; where drag = beta speed^2
;; and beta = 1/2 C rho pi D^2/4
;; note that we are taking direction into account here

;; we need the mass of a baseball -- which is about .15 kg.

;; so now we just need to write a procedure that performs a simple integration
;; of these equations -- there are more sophisticated methods but a simple one 
;; is just to step along by some step size in t and add up the values

;; dx = u dt
;; dy = v dt
;; du = - 1/m speed beta u dt
;; dv = - (1/m speed beta v + g) dt

;; initial conditions
;; u_0 = V cos alpha
;; v_0 = V sin alpha
;; y_0 = h
;; x_0 = 0

;; we want to start with these initial conditions, then take a step of size dt
;; (which could be say 0.1) and compute new values for each of these parameters
;; when y reaches the desired point (<= 0) we stop, and return the distance (x)

(define drag-coeff 0.5)
(define density-boston 1.25)
(define density-denver 1.06); kg/m^3
(define mass .145)  ; kg
(define diameter 0.074)  ; m
(define (beta x) (* .5 drag-coeff x (* 3.14159 .25 (square diameter))))
(define beta-boston (beta density-boston))
(define beta-denver (beta density-denver))


(define integrate
  (lambda (x y u v dt g m beta)
    (if (<= y 0)
        (list x y u v)
        (integrate (+ x (* u dt))
                   (+ y (* v dt))
                   (+ u (- (* (/ 1 m)
                              (sqrt (+ (square u) (square v)))  
                              u
                              beta
                              dt)))
                   (+ v (* (- (+ (* (/ 1 m)
                              (sqrt (+ (square u) (square v)))  
                              v
                              beta)
                              g))
                           dt))
                   dt g m beta))))

(define travel-distance
  (lambda (elevation velocity angle)
    (car (integrate 0
                    elevation
                    (* (cos (degree2radian angle)) velocity)
                    (* (sin (degree2radian angle)) velocity)
                    0.01
                    gravity
                    mass
                    beta-boston))))

    

;; RUN SOME TEST CASES
(travel-distance 1 45 45)
; Boston: 92.2
; Denver: 99.8
(travel-distance 1 40 45)
; Boston: 81.7
; Denver: 87.7
(travel-distance 1 35 45)
; Boston: 70.3
; Denver: 74.9

(define distance-vs-angle-test
  (list
(travel-distance 1 45 65)
(travel-distance 1 45 60)
(travel-distance 1 45 55)
(travel-distance 1 45 50)
(travel-distance 1 45 49)
(travel-distance 1 45 48)
(travel-distance 1 45 47)
(travel-distance 1 45 46)
(travel-distance 1 45 45)
(travel-distance 1 45 40)
(travel-distance 1 45 35)
(travel-distance 1 45 34)
(travel-distance 1 45 33)
(travel-distance 1 45 32)
(travel-distance 1 45 31)
(travel-distance 1 45 30)
(travel-distance 1 45 25)
(travel-distance 1 45 20)
(travel-distance 1 45 15)))

;;When velocity equals 45 m/s, hits at an angle betwee 30 and 47 degrees will land over the fence
;(67.2 76.2 83.3 88.6 89.5 90.3 91.1 91.7 92.2 93.8  93.4  93.1  92.7 92.1 91.5 90.7 85.9 78.5 68.2)
;; what about Denver?
;(73.1 82.8 90.5 96.2 97.1 97.9 98.7 99.3 99.8 101.3 100.5 100.1 99.7 99.0 98.2 97.6 91.8 83.3 71.9)
;;In Denver, the range of angles that take the ball over the fence is 25 to 54 degrees


;; Problem 7
 
;; now let's turn this around.  Suppose we want to throw the ball.  The same
;; equations basically hold, except now we would like to know what angle to 
;; use, given a velocity, in order to reach a given height (receiver) at a 
;; given distance
(define check-angle
  (lambda (velocity elevation target-distance)
    (lambda (angle)
      (let ((actual-distance (travel-distance elevation velocity angle)))
        (< (abs (- actual-distance target-distance)) 1)))))

(define (integers-between a b)
  (if (eq? a b)
      (cons b '())
      (cons a (integers-between (+ a 1) b))))

(define integrate-with-time
  (lambda (x y u v dt g m beta t)
    (if (<= y 0)
        (list t x y u v)
        (integrate-with-time (+ x (* u dt))
                             (+ y (* v dt))
                             (+ u (- (* (/ 1 m)
                                        (sqrt (+ (square u) (square v)))  
                                        u
                                        beta
                                        dt)))
                             (+ v (* (- (+ (* (/ 1 m)
                                              (sqrt (+ (square u) (square v)))  
                                              v
                                              beta)
                                           g))
                                     dt))
                             dt g m beta
                             (+ t dt)))))

(define travel-time
  (lambda (elevation velocity angle)
    (car (integrate-with-time 0
                              elevation
                              (* (cos (degree2radian angle)) velocity)
                              (* (sin (degree2radian angle)) velocity)
                              0.01
                              gravity
                              mass
                              beta-boston
                              0))))

(define find-optimal-angle
  (lambda (velocity target-distance)
    (let ((reaches-target? (check-angle velocity 1 target-distance)))
      (let ((viable-angles (filter reaches-target? (integers-between -90 90))))
        (define (help angles best-yet)
          (let ((best-time-yet (travel-time 1 velocity best-yet)))
            (cond ((null? angles) (list best-yet best-time-yet))
                  ((< (travel-time 1 velocity (car angles)) best-time-yet)
                   (help (cdr angles) (car angles)))
                  (else (help (cdr angles) best-yet)))))
        (if (null? viable-angles)
            #f
            (help (cdr viable-angles) (car viable-angles)))))))
          

;; a cather trying to throw someone out at second has to get it roughly 36 m
;; (or 120 ft) how quickly does the ball get there, if he throws at 55m/s,
;;  at 45m/s, at 35m/s?
(find-optimal-angle 55 36)
;7.6s
(find-optimal-angle 45 36)
;6.8s
(find-optimal-angle 35 36) 
;1.3s

;; try out some times for distances (30, 60, 90 m) or (100, 200, 300 ft) 
;; using 45m/s
(define (test-30-60-90m velocity)
  (list
   (find-optimal-angle velocity 30)
   (find-optimal-angle velocity 60)
   (find-optimal-angle velocity 90)))

;; at 45 m/s (0.8s, 1.8s, 3.6s)
;; at 35 m/s (1s, 2.5s, #f)



;; Problem 8
;; As was demonstrated, am outfielder throwing at 35m/s cannot get the ball 90m in the air.
;; He has to bounce it there
 
(define travel-distance-with-bounce
  (lambda (elevation velocity angle no-bounces)
    (define (bouncing-distance bounce-velocity bounces)
      (let ((first-bounce (travel-distance 0.01 bounce-velocity angle)))
        (if (= bounces 1)
            first-bounce
            (+ first-bounce (bouncing-distance (* 0.5 bounce-velocity) (- bounces 1))))))
    (+ (travel-distance elevation velocity angle)
       (bouncing-distance (* 0.5 velocity) no-bounces))))

(travel-distance-with-bounce 1 35 45 1)

(define how-many-bounces-needed
  (lambda (velocity target-distance)
    (define (try-with-bounces velocity distance no-bounces)
      (if (null? (filter (lambda (y) (>= y target-distance))
                         (map (lambda (x) (travel-distance-with-bounce 1 velocity x no-bounces))
                              (integers-between -90 90))))
          (try-with-bounces velocity distance (+ no-bounces 1))
          no-bounces))
    (if (find-optimal-angle velocity target-distance)
        0
        (try-with-bounces velocity target-distance 1))))

(how-many-bounces-needed 35 90)
(how-many-bounces-needed 35 100)

(define total-distance
  (lambda (elevation velocity angle)
    (define (total-with-bounces bounces)
      (let ((distance1 (travel-distance-with-bounce elevation velocity angle bounces))
            (distance2 (travel-distance-with-bounce elevation velocity angle (+ bounces 1))))
        (if (< (abs (- distance1 distance2)) 0.5)
            distance1
            (total-with-bounces (+ bounces 1)))))
    (total-with-bounces 1)))

(total-distance 1 35 40)

;; Problem 9
(define bounce-velocity
  (lambda (initial-elevation initial-velocity angle)
    (let ((impact-point (integrate 0
                                   (if (= initial-elevation 0) 0.01 initial-elevation)
                                   (* (cos (degree2radian angle)) initial-velocity)
                                   (* (sin (degree2radian angle)) initial-velocity)
                                   0.01
                                   gravity
                                   mass
                                   beta-boston)))
      (let ((velocity-components (cddr impact-point)))
        (sqrt (+ (square (car velocity-components))
                 (square (cadr velocity-components))))))))

(define new-travel-distance-with-bounce
  (lambda (elevation initial-velocity angle no-bounces)
    (define (bouncing-distance velocity bounces)
      (let ((first-bounce (travel-distance 0.01 velocity angle)))
        (if (= bounces 1)
            first-bounce
            (+ first-bounce (bouncing-distance (bounce-velocity 0.01 velocity angle) (- bounces 1))))))
    (+ (travel-distance elevation initial-velocity angle)
       (bouncing-distance (bounce-velocity elevation initial-velocity angle) no-bounces))))

#|(define a (bounce-velocity 1 35 45))
(define b (bounce-velocity 0 a 45))
(define c (bounce-velocity 0 b 45))
(define d (bounce-velocity 0 c 45))
(define e (bounce-velocity 0 d 45))
a
b
c
d
e|#
(travel-distance-with-bounce 1 35 45 1)
(new-travel-distance-with-bounce 1 35 45 1)
(new-travel-distance-with-bounce 1 35 45 2)
(new-travel-distance-with-bounce 1 35 45 3)
(new-travel-distance-with-bounce 1 35 45 4)
(new-travel-distance-with-bounce 1 35 45 5)
(new-travel-distance-with-bounce 1 35 45 6)
(new-travel-distance-with-bounce 1 35 45 7)
(new-travel-distance-with-bounce 1 35 45 8)
(new-travel-distance-with-bounce 1 35 45 9)
(new-travel-distance-with-bounce 1 35 45 10)
(new-travel-distance-with-bounce 1 35 45 11)
(new-travel-distance-with-bounce 1 35 45 12)
(new-travel-distance-with-bounce 1 35 45 13)
(new-travel-distance-with-bounce 1 35 45 14)
(new-travel-distance-with-bounce 1 35 45 15)
(new-travel-distance-with-bounce 1 35 45 16)
(new-travel-distance-with-bounce 1 35 45 17)
(new-travel-distance-with-bounce 1 35 45 18)
(new-travel-distance-with-bounce 1 35 45 19)
(new-travel-distance-with-bounce 1 35 45 20)

(define new-total-distance
  (lambda (elevation velocity angle)
    (define (total-with-bounces bounces)
      (let ((distance1 (new-travel-distance-with-bounce elevation velocity angle bounces))
            (distance2 (new-travel-distance-with-bounce elevation velocity angle (+ bounces 1))))
        (if (< (abs (- distance1 distance2)) 0.5)
            distance1
            (total-with-bounces (+ bounces 1)))))
    (total-with-bounces 1)))

;(total-distance 1 35 40)
;(new-total-distance 1 35 40)