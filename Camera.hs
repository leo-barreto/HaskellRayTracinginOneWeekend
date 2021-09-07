module Camera where

import Vec3
import Ray

-- Image
aspect_ratio = 16 / 9

-- Camera
vp_height = 2
vp_width = aspect_ratio * vp_height
focal_len = 1
origin_cam = (0, 0, 0)
horizontal = (vp_width, 0, 0)
vertical = (0, vp_height, 0)
lower_left_corner = v1 `subv` v2 where v1 = origin_cam `subv` (divscalar horizontal 2)
                                       v2 = (divscalar vertical 2) `sumv` (0, 0, focal_len)

get_ray :: Double -> Double -> Ray
get_ray x y = Ray origin_cam (a1 `sumv` a2)
              where a1 = lower_left_corner `sumv` (horizontal `mulscalar` x)
                    a2 = (vertical `mulscalar` y) `subv` origin_cam
