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
   (list 'total-availible (list "Total in manure"))
   (list 'crop-availible (list "Crop available nutrients (Total in manure)"))
   (list 'crop-availible-event (list "Crop available nutrients"))
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
   (list 'spring-barley-incorporated-feed (list "Spring barley, straw incorporated, feed"))
   (list 'spring-barley-incorporated-malt (list "Spring barley, straw incorporated, malt"))
   (list 'spring-barley-removed-feed (list "Spring barley, straw removed, feed"))
   (list 'spring-barley-removed-malt (list "Spring barley, straw removed, malt"))
   (list 'winter-wheat-incorporated-feed (list "Winter wheat, straw incorporated, feed")) 
   (list 'winter-wheat-incorporated-mill (list "Winter wheat, straw incorporated, mill")) 
   (list 'winter-wheat-removed-feed (list "Winter wheat, straw removed, feed"))
   (list 'winter-wheat-removed-mill (list "Winter wheat, straw removed, mill"))
   (list 'grass-cut (list "Grass cut"));; (yield 6-8k, conc 1.5, stock med)"))
   (list 'grass-grazed (list "Grass grazed"));; (yield 6-8k, conc 1.5, stock med)"))
   
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
   (list 'surface (list "Surface"))
   (list 'ploughed (list "Ploughed"))
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
   (list 'DM20 (list "20% DM"))
   (list 'DM40 (list "40% DM"))
   (list 'DM60 (list "60% DM"))
   (list 'DM80 (list "80% DM"))
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
   (list 'rain-high (list "High (>700mm)"))
   (list 'rain-medium (list "Medium (600-700mm)"))
   (list 'rain-low (list "Low (<600mm)"))

   (list 'fertiliser-costs (list "Your fertiliser costs"))
   (list 'costs-blurb (list "How much do you pay for your fertiliser? This is used to calculate your cost savings."))
   (list 'n-cost (list "N (£ per Kg)"))
   (list 'p-cost (list "P<sub><small>2</small></sub>O<sub><small>5</small></sub> (£ per Kg)"))
   (list 'k-cost (list "K<sub><small>2</small></sub>O (£ per Kg)"))
   (list 'n-cost-imperial (list "N (£ per unit)"))
   (list 'p-cost-imperial (list "P<sub><small>2</small></sub>O<sub><small>5</small></sub> (£ per unit)"))
   (list 'k-cost-imperial (list "K<sub><small>2</small></sub>O (£ per unit)"))

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
   (list 'manure-n (list "N Kg/t content (elemental)"))
   (list 'manure-p (list "P Kg/t content (elemental)"))
   (list 'manure-k (list "K Kg/t content (elemental)"))
   
   (list 'delete-are-you-sure (list "Are you sure you want to delete this?"))

   (list 'still-needed (list "Nutrients still needed"))

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

   (list 'expert (list "Expert crop select"))
   (list 'crop-select (list "Expert crop select"))

   (list 'crop-category (list "Main crop category"))
   (list 'grass-subtype (list "Grass subtype"))
   (list 'silage (list "Silage"))
   (list 'target-yield (list "Target yield"))
   (list 'cut (list "Cut"))
   (list 'DM4-5 (list "DM 4-5%"))
   (list 'DM7-9 (list "DM 7-9%"))
   (list 'DM9-12 (list "DM 9-12%"))
   (list 'DM12-15+ (list "DM 12-15+%"))
   (list 'DM4-5 (list "DM 4-5%"))
   (list 'DM5-7 (list "DM 5-7%"))
   (list 'DM6-8 (list "DM 6-8%"))
   (list 'DM7-9 (list "DM 7-9%"))
   (list 'DM9-12 (list "DM 9-12%"))
   (list 'DM10-13 (list "DM 10-13%"))
   (list 'DM12-15+ (list "DM 12-15+%"))
   (list 'established (list "Established"))
   (list 'sown (list "Sown"))
   (list 'summer-autumn (list "Summer-autumn"))
   (list 'clover (list "Clover"))
   (list 'grazed (list "Grazed"))
   (list 'hay (list "Hay"))
   (list 'rye (list "Rye"))
   (list 'grass (list "Grass"))

   (list 'accord (list "Accord"))
   (list 'annabelle (list "Annabelle"))
   (list 'anya (list "Anya"))  
   (list 'colmo (list "Colmo"))     
   (list 'estima (list "Estima"))
   (list 'innovator (list "Innovator"))
   (list 'maris-bard (list "Maris Bard"))
   
   (list 'atlantic (list "Atlantic"))
   (list 'amanda (list "Amanda"))
   (list 'arcade (list "Arcade"))
   (list 'carlinford (list "Carlingford"))
   (list 'charlotte (list "Charlotte"))
   (list 'courage (list "Courage")) 
   (list 'dunrod (list "Dundrod"))   
   
   (list 'agria (list "Agria")) 
   (list 'ambo (list "Ambo"))   
   (list 'amora (list "Amora"))
   (list 'cabaret (list "Cabaret"))
   (list 'caesar (list "Caesar")) 
   (list 'cosmos (list "Cosmos"))   
   (list 'cultra (list "Cultra"))   

   (list 'asterix (list "Asterix"))
   (list 'cara (list "Cara"))
   (list 'lady-balfour (list "Lady Balfour"))
   (list 'markies (list "Markies"))       
   (list 'royal (list "Royal")) 	       
   (list 'vales-everest (list "Vales Everest"))   
   (list 'vales-sovereign (list "Vales Sovereign"))

   (list 'under60 (list "<60 days"))
   (list '60-90 (list "60-90 days"))
   (list '90-120 (list "90-120 days"))
   (list 'over120 (list ">120 days"))

   (list 'growing-season-length (list "Length of growing season"))
   (list 'potatoes (list "Potatoes"))
   (list 'potato-group (list "Variety"))

   ))
