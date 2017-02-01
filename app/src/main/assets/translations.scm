;; -*- mode: scheme; -*-
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

(define i18n-text
  (list

   (list 'title (list "The Farm Crap App: Pro Edition"))
   (list 'splash-about (list "Manage your muck with the Farm Crap App"))
   (list 'splash-blurb (list "Developed by <a href='http://fo.am/kernow'>FoAM Kernow</a> on behalf of the <a href='www.swarmhub.co.uk'>SWARM Knowledge Hub</a>, a Rural Development Programme for England (RDPE) initiative managed by <a href='http://www.duchy.ac.uk/'>Duchy College Rural Business School</a>, in partnership with Rothamsted Research North Wyke."))
   (list 'splash-discl (list "The Farm Crap App offers information for guidance purposes only and is not intended to amount to professional advice or opinion. FoAM Kernow, Duchy College, and Rothamsted Research North Wyke cannot be held responsible for any losses or damage resulting from the use of information provided by this app."))
   (list 'splash-start (list "Get Started!"))

   (list 'choose-units (list "Choose units"))
   (list 'calculator (list "Calculator"))
   (list 'email (list "Email export"))
   (list 'about (list "About"))
   (list 'back (list "Back"))
   (list 'done (list "Done"))
   (list 'save (list "Save"))
   (list 'cancel (list "Cancel"))

   (list 'crap-calculator (list "Crap Calculator"))
   (list 'season (list "Season"))
   (list 'quality (list "Quality"))
   (list 'crop-availible (list "Crop available"))
   (list 'nutrient-n-metric (list "N Kg/ha"))
   (list 'nutrient-p-metric (list "P<sub><small>2</small></sub>O<sub><small>5</small></sub> Kg/ha"))
   (list 'nutrient-k-metric (list "K<sub><small>2</small></sub>O Kg/ha"))
   (list 'nutrient-n-imperial (list "N units/acre"))
   (list 'nutrient-p-imperial (list "P<sub><small>2</small></sub>O<sub><small>5</small></sub> units/acre"))
   (list 'nutrient-k-imperial (list "K<sub><small>2</small></sub>O units/acre"))
   (list 'cost-saving (list "Fertiliser Savings"))

   (list 'field-calc-blurb (list "Enter new crap spreading event"))
   (list 'date (list "Set date"))

   (list 'report-type (list "Manure type"))
   (list 'report-date (list "Date"))
   (list 'report-amount (list "Application rate"))
   (list 'report-quality (list "Quality"))
   (list 'report-application (list "Application type"))
   (list 'report-season (list "Season"))
   (list 'report-crop (list "Crop"))
   (list 'report-soil (list "Soil"))
   (list 'report-size (list "Size"))

   (list 'soil-type (list "Soil type"))
   (list 'crop-type (list "Crop type"))
   (list 'application-type (list "Application type"))
   (list 'previous-crop-type (list "Previous crop type"))
   (list 'field-size (list "Field size (hectares)"))
   (list 'field-size-i (list "Field size (acres)"))
   
   ;; crop types
   (list 'winter-wheat-removed (list "Winter wheat, straw removed"))
   (list 'winter-wheat-incorporated (list "Winter wheat, straw incorporated")) 
   (list 'spring-barley-removed (list "Spring barley, straw removed"))
   (list 'spring-barley-incorporated (list "Spring barley, straw incorporated"))
   (list 'grass-cut (list "Grass cut (yield 6-8k, conc 1.5, stock med)"))
   (list 'grass-grazed (list "Grass grazed (yield 6-8k, conc 1.5, stock med)"))
   
   ;; previous crop types
   (list 'cereals (list "Cereals"))
   (list 'oilseed (list "Oilseed rape"))
   (list 'potatoes (list "Potatoes"))
   (list 'sugarbeet (list "Sugar beet"))
   (list 'peas (list "Peas"))
   (list 'beans (list "Beans"))
   (list 'low-n-veg (list "Low N veg"))
   (list 'medium-n-veg (list "Medium N veg"))
   (list 'forage (list "Forage crops (cut)"))
   (list 'uncropped (list "Uncropped land"))
   (list 'grass-low-n (list "Grass (low N/1 or more cuts)"))
   (list 'grass-high-n (list "Grass (3-5yr, high N, grazed)"))
   (list 'grass-other (list "Any other grass"))
   
   ;; soil types
   (list 'sandyshallow (list "Sandy/Shallow"))
   (list 'peat (list "Peat"))
   (list 'organic (list "Organic (10-20% organic matter)"))
   (list 'mediumshallow (list "Medium/Shallow"))
   (list 'medium (list "Medium"))
   (list 'deepclay (list "Deep clay"))
   (list 'deepsilt (list "Deep silt"))

   ;; application types
   (list 'straight-surface (list "Straight to surface"))
   (list 'straight-ploughed (list "Straight and ploughed"))
   (list 'stored-spread (list "Stored to surface"))
   (list 'stored-ploughed (list "Stored and ploughed"))
   (list 'splash-surface (list "Splash plate/surface"))
   (list 'splash-incorporated (list "Splash plate/incorporated"))
   (list 'shoe-bar-spreader (list "Trailing shoe/dribble bar/band spreader"))
   (list 'shallow-injected (list "Shallow injected"))

   (list 'regular-organic (list "Do you regularly add organic manures?"))
   (list 'yes (list "Yes"))
   (list 'no (list "No"))
   (list 'grown-grass (list "Have you grown grass in the last 3 years (other than previous year)?"))
   

   (list 'autumn (list "Autumn"))
   (list 'winter (list "Winter"))
   (list 'spring (list "Spring"))
   (list 'summer (list "Summer"))

   (list 'fym-cattle (list "Cattle"))
   (list 'fym-pig (list "Pig"))
   (list 'fym-sheep (list "Sheep"))
   (list 'fym-duck (list "Duck"))
   (list 'fym-horse (list "Horse"))
   (list 'fym-goat (list "Goat"))

   (list 'layer (list "Layer manure"))
   (list 'broiler (list "Broiler litter"))
   (list 'metric (list "Metric"))
   (list 'imperial (list "Imperial"))
   (list 'DM2 (list "2% DM (Thin soup)"))
   (list 'DM4 (list "4% DM (Medium soup)"))
   (list 'DM6 (list "6% DM (Thick soup)"))
   (list 'DM10 (list "10% DM (Porridge)"))
   (list 'DM4 (list "4% DM (Thick soup)"))
   (list 'DM6 (list "6% DM (Porridge)"))
   (list 'green (list "Green compost"))
   (list 'green-food (list "Green and foodwaste"))

   (list 'fields (list "Your fields"))
   (list 'list-empty (list "Nothing yet"))
   (list 'field-name (list "Field name"))
   (list 'delete (list "Delete"))
   (list 'events (list "Spreading events"))
   (list 'fieldcalc-title (list "..."))
   (list 'fieldcalc-blurb (list "Enter new crap spreading event"))
   (list 'date-button (list "Set date"))
   (list 'date-text (list "Date"))
   (list 'manure-type (list "Manure type"))

   (list 'cattle (list "Cattle Slurry"))
   (list 'FYM (list "Farmyard Manure"))
   (list 'pig (list "Pig Slurry"))
   (list 'poultry (list "Poultry Litter"))
   (list 'compost (list "Compost"))
   (list 'custom-manure (list "Custom"))

   (list 'rainfall (list "Your farm's rainfall"))
   (list 'high (list "High"))
   (list 'medium (list "Medium"))
   (list 'low (list "Low"))

   (list 'fertiliser-costs (list "Your fertiliser costs"))
   (list 'costs-blurb (list "How much do you pay for your fertiliser? This is used to calculate your cost savings."))
   (list 'n-cost (list "N"))
   (list 'p-cost (list "P<sub><small>2</small></sub>O<sub><small>5</small></sub>"))
   (list 'k-cost (list "K<sub><small>2</small></sub>O"))

   ;; soil tests
   (list 'soil-test-p (list "P"))
   (list 'soil-test-k (list "K"))

   (list 'soil-p-0 (list "0"))
   (list 'soil-p-1 (list "1"))
   (list 'soil-p-2 (list "2"))
   (list 'soil-p-3 (list "3"))
   
   (list 'soil-k-0 (list "0"))
   (list 'soil-k-1 (list "1"))
   (list 'soil-k-2- (list "2-"))
   (list 'soil-k-2+ (list "2+"))
   (list 'soil-k-3 (list "3"))

   (list 'soil-info (list "Soil details"))
   (list 'crop-info (list "Crop details"))
   (list 'soil-test (list "Results of soil tests (if availible)"))
   (list 'soil-supply (list "Soil N supply"))
   (list 'crop-requirements (list "Crop nutrient requirements"))

   (list 'custom-manures (list "Your manures"))
   (list 'manures-blurb (list "Here you can add custom manure types"))
   (list 'manure-name (list "Manure name"))
   (list 'manure-n (list "N content"))
   (list 'manure-p (list "P<sub><small>2</small></sub>O<sub><small>5</small></sub> content"))
   (list 'manure-k (list "K<sub><small>2</small></sub>O content"))
   
   (list 'delete-are-you-sure (list "Are you sure you want to delete this?"))

   (list 'still-needed (list "Fertiliser still needed"))

   (list 'camera (list "Camera"))
   (list 'take-photo (list "Take photo"))
   (list 'load-gallery (list "Load gallery"))
   (list 'gallery (list "Gallery"))

   (list 'export (list "Export all data"))
   (list 'export-blurb (list "Email export all field data as a CSV spreadsheet file"))
   (list 'email-button (list "Email"))
   (list 'none (list "None"))

   (list 'graph-title (list "Crop available nutrients added to field"))
   (list 'factory-reset (list "Factory reset"))
   (list 'factory-reset-are-you-sure (list "Are you sure? This will permenantly delete everything."))

   ))
