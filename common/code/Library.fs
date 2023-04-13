namespace code

type Topping =
    | Pepperoni
    | Cheese
    | Pineapple

type ToppingAmount =
    | Light
    | Medium
    | Extra

type PizzaSize =
    | Small
    | Medium
    | Large

type Pizza =
    { Size: PizzaSize
      Toppings: (Topping * ToppingAmount) list }

type Order = Pizza list
