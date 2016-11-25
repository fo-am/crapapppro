;; all the manure types in metric

(define manure-tree
  '(type
    ((cattle 
      (quality 
       ((DM2 
         (nutrient 
          ((nitrogen
            (season		
             ((autumn
               (soil 
                ((sandyshallow (crop ((normal 8) (grass-oilseed 16))))
                 (mediumheavy (crop ((normal 48) (grass-oilseed 56)))))))
              (winter 48)
              (summer 72)
              (spring 56))))
           (phosphorous 30) 
           (potassium 220))))
        (DM6 
         (nutrient 
          ((nitrogen
            (season		
             ((autumn
               (soil 
                ((sandyshallow (crop ((normal 13) (grass-oilseed 26))))
                 (mediumheavy (crop ((normal 65) (grass-oilseed 78)))))))
              (winter 65)
              (summer 91)
              (spring 65))))
           (phosphorous 60) 
           (potassium 290))))
        (DM10
         (nutrient 
          ((nitrogen
            (season		
             ((autumn
               (soil 
                ((sandyshallow (crop ((normal 18) (grass-oilseed 36))))
                 (mediumheavy (crop ((normal 72) (grass-oilseed 90)))))))
              (winter 72)
              (summer 90)
              (spring 72))))
           (phosphorous 90) 
           (potassium 360)))))))
     
     (pig
      (quality
       ((DM2
         (nutrient 
          ((nitrogen
            (season		
             ((autumn
               (soil 
                ((sandyshallow (crop ((normal 15) (grass-oilseed 22.5))))
                 (mediumheavy (crop ((normal 52.5) (grass-oilseed 60)))))))
              (winter 60)
              (summer 82.5)
              (spring 82.5))))
           (phosphorous 25) 
           (potassium 90))))
        (DM4-pig
         (nutrient 
          ((nitrogen
            (season		
             ((autumn
               (soil 
                ((sandyshallow (crop ((normal 18) (grass-oilseed 27))))
                 (mediumheavy (crop ((normal 54) (grass-oilseed 63)))))))
              (winter 63)
              (summer 90)
              (spring 90))))
           (phosphorous 45) 
           (potassium 110))))
        (DM6-pig
         (nutrient 
          ((nitrogen
            (season		
             ((autumn
               (soil 
                ((sandyshallow (crop ((normal 22) (grass-oilseed 23))))
                 (mediumheavy (crop ((normal 55) (grass-oilseed 66)))))))
              (winter 69)
              (summer 99)
              (spring 99))))
           (phosphorous 65) 
           (potassium 125)))))))
     (poultry
      (quality
       ((layer
         (nutrient 
          ((nitrogen
            (season		
             ((autumn
               (soil 
                ((sandyshallow (crop ((normal 19) (grass-oilseed 28.5))))
                 (mediumheavy (crop ((normal 47.5) (grass-oilseed 57)))))))
              (winter 47.5)
              (summer 66.5)
              (spring 66.5))))
           (phosphorous 84) 
           (potassium 86)))))))
     (compost
      (quality
       ((green (nutrient ((nitrogen 7.5) (nitrogen-avail 0.2) (phosphorous 3) (potassium 5.5))))
        (green-food (nutrient ((nitrogen 11) (nitrogen-avail 0.6) (phosphorous 3.8) (potassium 8.0))))))))
        
    ))
