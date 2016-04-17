newtype Kelvin = Kelvin { getKelvin :: Double } deriving (Show)
newtype Celsius = Celsius { getCelsius :: Double } deriving (Show)

type Thermostat a = (Kelvin, Kelvin -> a)

kelvinToCelsius :: Kelvin -> Celsius
kelvinToCelsius (Kelvin k) = Celsius $ k - 273.15

initThermostat :: Thermostat Celsius
initThermostat = (Kelvin 298.15, kelvinToCelsius)

up :: Thermostat a -> a
up (Kelvin k, f) = f $ Kelvin (k + 1)

down :: Thermostat a -> a
down (Kelvin k, f) = f $ Kelvin (k - 1)

toString :: Thermostat Celsius -> String
toString (k, f) = (show $ getCelsius $ f k) ++ " Celsius"

extract :: Thermostat a -> a
extract (k, f) = f k

extend :: (Thermostat a -> b) -> Thermostat a -> Thermostat b
extend p (k, f) = (k, \k' -> p (k', f))

main :: IO ()
main =
  putStrLn $ show $ extract $ extend down $ extend down initThermostat

---------------------------------------

--extend up (t, f) = (t, \t' -> up (t', f)) = (t, \t' -> f (t' + 1))

--up' (t, f) = (t + 1, f)

up' :: Thermostat a -> Thermostat a
up' (Kelvin k, f) = (Kelvin (k + 1), f)

down' :: Thermostat a -> Thermostat a
down' (Kelvin k, f) = (Kelvin (k - 1), f)

