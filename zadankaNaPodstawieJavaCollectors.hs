import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Char as Char
import Data.Function (on)


data City = City {cityName :: String, code:: Int} deriving (Eq, Show)

lublin = City "Lublin" 20
swidnik = City "Swidnik" 10
radom = City "Radom" 1
elk = City "Elk" 22
mediolan = City "Mediolan" (-2)

data Person = Person {name :: String, lastName::  String, birthYear:: Int, cities:: [City]} deriving (Eq, Show)

karolNowak = Person "Karol" "Nowak" 1988 [lublin]
galAnonim = Person "Gal" "Anonim" 1066 [elk, swidnik]
gertrudaAdamiak = Person "Gertruda" "Adamiak" 1950 [lublin, mediolan]
janKowalski = Person "Jan" "Kowalski" 1950 [radom, radom]
dawidMroz = Person "Dawid" "Mroz" 1969 [radom]
tadeuszNowak = Person "Tadeusz" "Nowak" 1980 [radom, elk]
dawidNowak = Person "Dawid" "Nowak" 2005 [lublin, swidnik]
grazynaDebska = Person "Grazyna" "Debska" 1969 [radom, lublin]

people = [karolNowak, galAnonim, gertrudaAdamiak, janKowalski, dawidMroz, tadeuszNowak, grazynaDebska]


shouldCreateMapWithBirthYearAsKeyAndNameLengthSumAsValue =
  let lengthOfName = length . name in
    let people2YearAndNameLength = List.map (\p -> (birthYear p, lengthOfName p)) people in
      Map.fromListWith (+) people2YearAndNameLength

shouldCreateMapWithFirstNameLetterLowercaseAsKeyAndListOfSurnamesAsValue =
  let firstLetterOfNameLowercased = Char.toLower . head . name in
    let people2Tuple = List.map (\p -> (firstLetterOfNameLowercased p, [lastName p])) people in
      Map.fromListWith (++) people2Tuple


shouldReturnListOfPeopleWithCitiesWithHighestAreaCode =
  let maxCityCode cities = code (List.maximumBy (compare `on` code) cities) in
    let personWithMaxCityCode p = (p, maxCityCode (cities p)) in
      let peopleTuplesSortedByMaxCityCode = List.sortBy (flip (compare `on` snd)) (List.map personWithMaxCityCode people) in
        let peopleTuplesGroupedByMaxCityCode = List.groupBy ((==) `on` snd) peopleTuplesSortedByMaxCityCode in
          let peopleTuplesWithMaxCityCode = head peopleTuplesGroupedByMaxCityCode in
            List.map fst peopleTuplesWithMaxCityCode


