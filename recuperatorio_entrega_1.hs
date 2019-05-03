import Text.Show.Functions
import Data.List

type Truco = Auto -> Auto --function

data Auto = Auto {
	nombre :: [Char],
	nafta :: Double,
	velocidad :: Integer,
	enamorado :: [Char],
	truco :: Truco
} deriving (Show)


auto1 = Auto {
	nombre = "RochaMcQueen",
	nafta = 300,
	velocidad = 0,
	enamorado = "Ronco",
	truco = deReversaRocha
}

auto2 = Auto {
	nombre = "Biankerr",
	nafta = 500,
	velocidad = 20,
	enamorado = "Tinch",
	truco = impresionar
}

auto3 = Auto {
	nombre = "Gushtav",
	nafta = 200,
	velocidad = 130,
	enamorado = "PetiLaLinda",
	truco = nitro
}

auto4 = Auto {
	nombre = "Rodra",
	nafta = 0,
	velocidad = 50,
	enamorado = "Taisa",
	truco = fingirAmor "Petra"
}

--truco1
deReversaRocha :: Auto -> Auto
deReversaRocha (Auto nombre nafta velocidad enamorado truco) =  (Auto nombre ((1000*0.2)+nafta) velocidad enamorado truco)

--truco2
impresionar :: Auto -> Auto
impresionar (Auto nombre nafta velocidad enamorado truco ) = (Auto nombre nafta (velocidad*2) enamorado truco)

--truco3
nitro :: Auto -> Auto
nitro (Auto nombre nafta velocidad enamorado truco ) = (Auto nombre nafta (velocidad+15) enamorado truco)

--truco4
fingirAmor :: [Char] -> Auto -> Auto
fingirAmor nuevoEnamorado ( Auto nombre nafta velocidad enamorado truco) = ( Auto nombre nafta velocidad nuevoEnamorado truco )


--Chequear que un auto pueda realizar un truco
criterioTruco :: Auto -> Bool
criterioTruco (Auto nombre nafta velocidad enamorado truco) = (>0) nafta && (<100) velocidad
    

--Copiar truco de otro auto
copiarTruco :: Auto -> Auto -> Auto
copiarTruco (Auto nombre autoUnoNafta velocidad enamorado autoUnoTruco) (Auto a autoDosNafta b c autoDosTruco) 	 | criterioTruco (Auto nombre autoUnoNafta velocidad enamorado autoUnoTruco) = (Auto nombre autoUnoNafta velocidad enamorado autoDosTruco)
																							 	 				 | 		    otherwise = (Auto nombre autoUnoNafta velocidad enamorado autoUnoTruco)

--Varios autos pueden realizar el truco
realizarTruco :: Auto -> Auto
realizarTruco (Auto  nombre nafta velocidad enamorado truco) = truco (Auto nombre nafta velocidad enamorado truco)

trucoActivado :: Auto -> Auto
trucoActivado (Auto nombre nafta velocidad enamorado truco ) | criterioTruco (Auto nombre nafta velocidad enamorado truco) = realizarTruco (Auto nombre nafta velocidad enamorado truco)
		   													 | 		  otherwise = (Auto nombre nafta velocidad enamorado truco)



		     
	

