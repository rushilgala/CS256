-- The type declarations for Day, Month and Year are type Int
type Month = Int
type Day = Int
type Year = Int
{- Explanation on function days
   ============================
   To solve the leap year problem, I have used a function called isLeapYear
   The function returns True if it is a leap year, and false if not.
	 The days function checks the month integer to return the number of days if it is in an element in the list
	 To integrate the days function and the isLeapYear function, I have created a new function called days_wrap
	 This function takes the year and month as its input so that leap years can be handled correctly
	 If the year is leap year and the month provided is 2 (February) then 1 is added to the output of the days function, giving 29 days for February on a leap year
	 The definition of a leap year is that the year must either be (year modulo 4 is 0 AND year modulo 100 is not 0) OR (year modulo 400 is 0)
-}
isLeapYear :: Int -> Bool
isLeapYear year
  | ((year `mod` 4) == 0) && (not (year `mod` 100 == 0)) || (year `mod` 400 == 0)	= True
  | otherwise																																			= False
-- days function gets the month as an integer, checks whether it is an element of a predefined list and returns correct number of days in the month
days :: Month -> Int
days m
 | m `elem` [1,3,5,7,8,10,12] = 31
 | m `elem` [4,6,9,11] 				= 30
 | m `elem` [2] 							= 28
 | otherwise 									= error("Invalid month")
{-
		days_wrap function input is the year and the month, and handles invalid input as well as leap year handling
		=======================
		TESTING
		=======================
		INPUT: days_wrap 2016 2 => Returns 29 (leap year handling)
		INPUT: days_wrap 2015 2 => Returns 28 (non-leap year handling)
		INPUT: days_wrap 2016 14 => Returns "Invalid month"
		INPUT: days_wrap 2016 3 => Returns 31 (normal month with 31 days)
		INPUT: days_wrap 2015 4 => Returns 30 (normal month with 30 days)
		INPUT: days_wrap 1900 2 => Returns 28 (Official definition of a leap year does not allow this as a leap year)
-}
days_wrap :: Year -> Month -> Int
days_wrap y m
 | m < 1 || m > 12								= error("Invalid month")
 | y < 0													= error("Invalid year")
 | isLeapYear y == True && m == 2	= (+) 1 $ days m
 | otherwise											= days m
{- Explanation on type Date
   ========================
   The type Date holds 3 properties: day with type Day, month with type Month and year with type Year.
   This can be used with other custom defined functions.
-}
data Date = Date { day :: Day, month :: Month, year :: Year } deriving (Read)
-- Showing the date will revert to showd function used to print the date in the format "dd/mm/yyyy"
instance Show Date where
	show = showd
-- Eq is determining whether date 1 and date 2 are equal, therefore have the same Day, Month and Year.
instance Eq Date where
	Date {day = d1, month = m1, year = y1} == Date {day = d2, month = m2, year = y2} = (d1 == d2) && (m1 == m2) && (y1 == y2)
-- Ord allows the order of dates to be determined. This compares the years followed by the months followed by the days
instance Ord Date where
	Date {day = d1, month = m1, year = y1} <= Date {day = d2, month = m2, year = y2}
		| y1 < y2																	= True
		| (y1 == y2) && (m1 < m2) 								= True
		| (y1 == y2) && (m1 == m2) && (d1 <= d2) 	= True
		| otherwise 															= False
{-
	addDate creates a new instance of the Date to be used later
	==============================
	TESTING
	==============================
	INPUT: addDate 0 12 2015 => Returns "Invalid day"
	INPUT: addDate 32 1 2016 => Returns "Invalid day"
	INPUT: addDate 31 4 2016 => Returns "Invalid day"
	INPUT: addDate 31 1 2016 => Returns 31/1/2016
	INPUT: addDate 30 4 2016 => Returns 30/4/2016
	INPUT: addDate 32 2 2016 => Returns "Invalid day"
	INPUT: addDate 29 2 2016 => Returns 29/2/2016
	INPUT: addDate 29 2 2015 => Returns "Invalid day"
	INPUT: addDate 28 2 2015 => Returns 28/2/2015
	INPUT: addDate 12 14 2016 => Returns "Invalid month"
	INPUT: addDate 17 11 2016 => Returns 17/11/2016
-}
addDate :: Day -> Month -> Year -> Date
addDate d m y
	| d < 1																			= error("Invalid day")
	| m `elem` [1,3,5,7,8,10,12] && d > 31 			= error("Invalid day")
	|	m `elem` [4,6,9,11] && d > 30 						= error("Invalid day")
	| isLeapYear y == True && m == 2 && d > 29 	= error("Invalid day")
	| isLeapYear y == False && m == 2 && d > 28 = error("Invalid day")
	| m < 1 || m > 12														= error("Invalid month")
	| y < 0																			= error("Invalid year")
	| otherwise																	= Date d m y
{- Explanation on function showd
   =============================
   Prints a string of the date in the format "dd/mm/yyyy"
-}
showd :: Date -> [Char]
showd (Date {day = d, month = m, year = y})
	| d < 10 && m < 10 	= "0"++show d++"/0"++show m++"/"++show y
	| d < 10 && m > 9		= "0"++show d++"/"++show m++"/"++show y
	| d > 9 && m < 10		= show d++"/0"++show m++"/"++show y
	| otherwise					= show d++"/"++show m++"/"++show y
getDay :: Date -> Int
getDay (Date {day = d, month = m, year = y}) = d
getMonth :: Date -> Int
getMonth (Date {day = d, month = m, year = y}) = m
getYear :: Date -> Int
getYear (Date {day = d, month = m, year = y}) = y
{- Explanation on function before
   ==============================
   The before function takes in two dates and uses Ord from the Date type to determine if date 1 was before date 2, returning true or false
	 ==============================
	 TESTING
	 ==============================
	 INPUT: before(addDate 25 12 2016, addDate 25 12 2016) => Returns True // Equal Dates
	 INPUT: before(addDate 25 12 2015, addDate 25 12 2016) => Returns True // Correct year check
	 INPUT: before(addDate 25 12 2017, addDate 25 12 2016) => Returns False // Incorrect year check
	 INPUT: before(addDate 25 09 2016, addDate 25 12 2016) => Returns True // Correct month check
	 INPUT: before(addDate 25 12 2016, addDate 25 09 2016) => Returns False // Incorrect month check
	 INPUT: before(addDate 24 12 2016, addDate 25 12 2016) => Returns True // Correct day check
	 INPUT: before(addDate 25 12 2016, addDate 24 12 2016) => Returns False // Incorrect day check
	 INPUT: before(addDate 25 05 2016, addDate 01 12 2016) => Returns True // Correct day and month check
-}
before :: (Date,Date) -> Bool
before (d1, d2) = d1 <= d2
{- Explanation on type Event
   =========================
   The function events takes the inputs Date, Form (as a String), and Comment (as a String)
   =========================
   TESTING
   =========================
   INPUT: Event (addDate 27 11 2015) 1 "In my DCS office. Security and Paramedics called."
   INPUT: Event (addDate 10 12 2015) 1 "After a sleepless night, in my kitchen."
   INPUT: Event (addDate 17 12 2015) 1 "On the phone, on the couch."
   INPUT: Event (addDate 01 01 2016) 1 "Napping in bed this morning."
   INPUT: Event (addDate 05 02 2016) 1 "In my DCS office."
   INPUT: Event (addDate 02 04 2016) 1 "Earlson, Beechwood Avenue. Paramedics called."
   INPUT: Event (addDate 13 04 2016) 1 "In the Gents toilets on the 3rd floor of DCS."
   INPUT: Event (addDate 17 04 2016) 1 "At the Friends Meeting House (Coventry)."
   INPUT: Event (addDate 08 05 2016) 1 "In Providence St on my way to the Meeting House."
   INPUT: Event (addDate 13 05 2016) 1 "In the Arts Centre Cafe."
   INPUT: Event (addDate 09 06 2016) 1 "In my DCS office."
   INPUT: Event (addDate 23 06 2016) 1 "In my DCS office."
   INPUT: Event (addDate 15 07 2016) 1 "In my DCS office."
   INPUT: Event (addDate 19 07 2016) 1 "In my DCS office."
   INPUT: Event (addDate 09 08 2016) 1 "In Jin's Cafe, Cannon Park Shops."
   INPUT: Event (addDate 24 08 2016) 1 "In the Arts Centre Cafe."
   INPUT: Event (addDate 10 09 2016) 1 "At home in my bedroom."
   INPUT: Event (addDate 25 09 2016) 1 "At home in my armchair."
   INPUT: Event (addDate 25 10 2016) 1 "At home, having just got up."
   INPUT: Event (addDate 31 10 2016) 1 "At home, an hour after getting up."
-}
data Event = Event { date :: Date, form :: Int, comment :: [Char] } deriving (Read)
showe :: Event -> [Char]
showe (Event {date = d, form = f, comment = c}) = "Date: "++show d++"\tForm:"++show f++"\tComment: "++show c++"\n"
instance Show Event where
	show = showe
instance Eq Event where
  Event {date = d1, form = f1, comment = c1} == Event {date = d2, form = f2, comment = c2}
    | d1 == d2  = True
    | otherwise = False
instance Ord Event where
  Event {date = d1, form = f1, comment = c1} <= Event {date = d2, form = f2, comment = c2}
    | before(d1, d2) == True  = True
    | otherwise               = False
--- List of events
type Events = [Event]
events :: Events
events = [Event (addDate 27 11 2015) 1 "In my DCS office. Security and Paramedics called.",
					Event (addDate 10 12 2015) 1 "After a sleepless night, in my kitchen.",
					Event (addDate 17 12 2015) 1 "On the phone, on the couch.",
					Event (addDate 01 01 2016) 1 "Napping in bed this morning.",
					Event (addDate 05 02 2016) 1 "In my DCS office.",
					Event (addDate 02 04 2016) 1 "Earlson, Beechwood Avenue. Paramedics called.",
					Event (addDate 13 04 2016) 1 "In the Gents toilets on the 3rd floor of DCS.",
					Event (addDate 17 04 2016) 1 "At the Friends Meeting House (Coventry).",
					Event (addDate 08 05 2016) 1 "In Providence St on my way to the Meeting House.",
					Event (addDate 13 05 2016) 1 "In the Arts Centre Cafe.",
					Event (addDate 09 06 2016) 1 "In my DCS office.",
					Event (addDate 23 06 2016) 1 "In my DCS office.",
					Event (addDate 15 07 2016) 1 "In my DCS office.",
					Event (addDate 19 07 2016) 1 "In my DCS office.",
					Event (addDate 09 08 2016) 1 "In Jin's Cafe, Cannon Park Shops.",
					Event (addDate 24 08 2016) 1 "In the Arts Centre Cafe.",
					Event (addDate 10 09 2016) 1 "At home in my bedroom.",
					Event (addDate 25 09 2016) 1 "At home in my armchair.",
					Event (addDate 25 10 2016) 1 "At home, having just got up.",
					Event (addDate 31 10 2016) 1 "At home, an hour after getting up."]


-- Gets the date of a specific event
eventToDate :: Event -> Date
eventToDate (Event {date = d, form = f, comment = c}) = d
{-
	Inserting an Event
	==================
  This function recursively adds events depending on their date, therefore keeping a sorted list by date (earliest first)
  If the date of the element we are adding is 'older' then we look at the next element
  Otherwise, we insert it before the current element
  Effectively, this is an insertion sort
  ==================
	TESTING
	==================
	Inserting an Event to the list of Events (sorts it by the date)
	INPUT: insertEvent (Event (addDate 30 11 2015) 1 "In my DCS office. Security and Paramedics called.") events => Places the event after 27/11/2015 event, and before 10/12/2015 event
	INPUT: insertEvent (Event (addDate 25 11 2015) 1 "In my DCS office. Security and Paramedics called.") events => Places the event at the head of the list
-}
insertEvent :: Event -> [Event] -> [Event]
insertEvent x [] = [x]
insertEvent x (y:ys)
	| x > y    	= y : insertEvent x ys
	| otherwise	= x : y : ys
{- Explanation on variable history
   ===============================
   History is a list of events. Every time events is called, the event is added to the list events.
	 1) The tail of the list will contain the the most recent event date
	 2) To get the number of days since the last most recent event, traverse through the list
	 and calculate the difference between the last and 2nd last date
	 3) av_gaps can be calculated by the difference in dates of the first date (27/11/2015) and the most recent date
	 divided by the (length of events - 1)
-}
type History = [Char]
history :: History
history = "(1) The date of the most recent event: "++showd(eventToDate(last events))++". \
\(2) The gap between the most recent event and the one before it: "++display(calcDiff(eventToDate(events !! (length events-2)),eventToDate(last events)))++". \
\(3) The average gap between events from the earliest date and latest event: "++display(av_gaps)++"."
-- Display an integer as a string of characters (useful for history)
display :: Int -> [Char]
display x = show x
{-
	Calculate the difference between two dates by converting them to Julian dates and subtracting them from one another
-}
calcDiff :: (Date,Date) -> Int
calcDiff (a,b)
	| (before(a,b)) == False 	= error("Date 1 is not before Date 2")
	| otherwise			= round(convert2(convertToJulian(b)) - convert2(convertToJulian(a)))

convertToJulian :: Date -> Float
convertToJulian d = 1720994.5 + bValue(aValue(yearp(getMonth d, getYear d))) + cValue(yearp(getMonth d, getYear d)) + dValue(monthp(getMonth d, getYear d)) + fromIntegral (getDay d)
{-
	Calculating functions used to work out the Julian date
-}
-- =================== Start of Julian Date Calculations ===============
-- Work out yearp
yearp :: (Month, Year) -> Int
yearp (m,y)
	| (m == 1) && (m == 2)		= y - 1
	| otherwise			= y
-- Work out monthp
monthp :: (Month, Year) -> Int
monthp (m,y)
	| (m == 1) && (m == 2)          = m + 12
        | otherwise                     = m
-- Work out A
aValue :: Int -> Int
aValue yp = truncate (fromIntegral (yp) / 100)
-- Work out B
bValue :: Int -> Float
bValue a = fromIntegral (2 - a + truncate(fromIntegral (a) / 4))
-- Work out C
cValue :: Int -> Float
cValue yp
	| (yp < 0)		= fromIntegral (truncate ((365.25 * fromIntegral (yp) - 0.75)))
	| otherwise		= fromIntegral (truncate (365.25 * fromIntegral (yp)))
-- Work out D
dValue :: Int -> Float
dValue mp = fromIntegral (truncate (30.6001 * (fromIntegral (mp) + 1)))
-- Convert Julian date into Modified Julian Date
convert2 :: Float -> Float
convert2 f = f - 2400000.5
-- ================ End of Julian Date calculations ===========
{-
	Get the average number of days between events. To work this out, we get the number of days from the start, and divide them by the number of items in the list events
-}
av_gaps :: Int
av_gaps = round (fromIntegral (calcDiff(eventToDate(head events),eventToDate(last events))) / fromIntegral (length events - 1)) :: Int
