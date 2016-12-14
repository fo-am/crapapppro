;; Farm Crap App Pro Copyright (C) 2016 FoAM Kernow
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; geo stuff
(define earth_radius 6371009) ;; in meters
(define pi 3.14159265359)
(define (degrees->radians d) (* d 0.0174533))
(define lat_dist (* pi (/ earth_radius 180.0)))
(define (vx v) (list-ref v 0))
(define (vy v) (list-ref v 1))

;; somewhere in france 
(define centre-lat 49.198935)
(define centre-lon 2.988281)

(define (wrap-ref l i)
  (list-ref l (modulo i (length l))))

(define (reproject latlng)
  (list (* (vx latlng) lat_dist)
	(* (vy latlng) lat_dist 
	   (cos (degrees->radians (vx latlng))))))

(define (area-of-polygon points)
  (define (_ i r)
    (cond
     ((> i (- (length points) 1)) r)
     (else
      (_ (+ i 1)
	 (+ r (* (vx (wrap-ref points i))
		 (- (vy (wrap-ref points (+ i 1)))
		    (vy (wrap-ref points (- i 1))))))))))
  (if (zero? (length points)) 
      0
      (abs (/ (_ -1 0) 2.0))))

;; not quite sure of this yet, needs checking properly
(define (recentre polygon)
  (map
   (lambda (p)
     (list (- (vx (car polygon)) (vx p))
           (- (vy (car polygon)) (vy p))))
   polygon))

(define (area-metres polygon)
  (area-of-polygon
   (recentre
    (map reproject polygon))))

(define (polygon-centroid polygon)
  (let ((t (foldl
            (lambda (latlng r)
              (list (+ (vx r) (vx latlng))
                    (+ (vy r) (vy latlng))))
            (list 0 0)
            polygon)))
    (if (zero? (length polygon))
	(list 0 0)
	(list (/ (vx t) (length polygon))
	      (/ (vy t) (length polygon))))))

(define (filter-empty-polys p)
  (filter
   (lambda (p)
     (not (zero? (length p))))
   p))

(define (polygons-centroid polygons)
  (let ((polygons (filter-empty-polys polygons)))
    (let ((t (foldl
	      (lambda (polygon r)     
		(let ((latlng (polygon-centroid polygon)))
		  (list (+ (vx r) (vx latlng))
			(+ (vy r) (vy latlng)))))
	      (list 0 0)
	      polygons)))
      (if (zero? (length polygons))
	  (list centre-lat centre-lon)
	  (list (/ (vx t) (length polygons))
		(/ (vy t) (length polygons)))))))


