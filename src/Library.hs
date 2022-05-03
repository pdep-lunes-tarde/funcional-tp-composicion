module Library where
import PdePreludat

data Ingrediente =
    Carne | Pan | Panceta | Cheddar | Pollo | Curry | QuesoDeAlmendras
    deriving (Eq, Show)

precioIngrediente Carne = 20
precioIngrediente Pan = 2
precioIngrediente Panceta = 10
precioIngrediente Cheddar = 10
precioIngrediente Pollo =  10
precioIngrediente Curry = 5
precioIngrediente QuesoDeAlmendras = 15

data Hamburguesa = Hamburguesa {
    precioBase :: Number,
    ingredientes :: [Ingrediente]
} deriving (Eq, Show)

precio :: Hamburguesa -> Number
precio hamburguesa = precioBase hamburguesa + precioTotal (ingredientes hamburguesa)

precioTotal :: [Ingrediente] -> Number
-- precioTotal ingredientes = sum (map precioIngrediente ingredientes)
precioTotal ingredientes = (sum . map precioIngrediente) ingredientes -- esta bien
-- precioTotal ingredientes = sum . map precioIngrediente $ ingredientes -- esta bien
-- haskell lo va a interpretar como:
-- (sum . map precioIngrediente) $ ingredientes

-- precioTotal ingredientes = sum . map precioIngrediente ingredientes -- esta mal
-- haskell lo va a interpretar como
-- sum . (map precioIngrediente ingredientes) 

agrandar :: Hamburguesa -> Hamburguesa
agrandar hamburguesa = agregarIngrediente (tipoDePati hamburguesa) hamburguesa

agregarIngrediente :: Ingrediente -> Hamburguesa -> Hamburguesa
agregarIngrediente ingrediente hamburguesa =
    Hamburguesa (precioBase hamburguesa) (ingrediente : ingredientes hamburguesa)

tipoDePati hamburguesa = primeroQueSeaTipoDePati (ingredientes hamburguesa)

esTipoDePati ingrediente = ingrediente == Pollo || ingrediente == Carne

primeroQueSeaTipoDePati ingredientes = head (filter esTipoDePati ingredientes)

descuento :: Number -> Hamburguesa -> Hamburguesa
descuento porcentaje hamburguesa =
    Hamburguesa (precioBase hamburguesa - precioBase hamburguesa * (porcentaje / 100))
                (ingredientes hamburguesa)

-- cuartoDeLibra es una hamburguesa de pan, carne, cheddar, pan y el precio base es de 20.

cuartoDeLibra :: Hamburguesa
cuartoDeLibra = Hamburguesa 20 [Pan, Carne, Cheddar, Pan]

-- la pdepBurguer que es un cuarto de libra agrandado 2 veces con panceta, cheddar y 20% de descuento. Su precio final deberia ser 110
pdepBurger :: Hamburguesa
pdepBurger =
    (descuento 20 . agregarIngrediente Cheddar . agregarIngrediente Panceta . agrandar . agrandar) cuartoDeLibra

-- pdepBurger = descuento 20 (agregarIngrediente Cheddar (agregarIngrediente Panceta (agrandar (agrandar cuartoDeLibra))))
