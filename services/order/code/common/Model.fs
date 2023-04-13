namespace common

open System

type Topping =
    | Pepperoni
    | Cheese
    | Pineapple

    static member tryFromString value =
        match value with
        | _ when String.Equals(value, nameof (Topping.Pepperoni)) -> Result.Ok Topping.Pepperoni
        | _ when String.Equals(value, nameof (Topping.Cheese)) -> Result.Ok Topping.Cheese
        | _ when String.Equals(value, nameof (Topping.Pineapple)) -> Result.Ok Topping.Pineapple
        | _ -> Result.Error $"'{value}' is not a valid topping."

    static member toString topping =
        match topping with
        | Topping.Pepperoni -> "Pepperoni"
        | Topping.Cheese -> "Cheese"
        | Topping.Pineapple -> "Pineapple"

type ToppingAmount =
    | Light
    | Medium
    | Extra

    static member tryFromString value =
        match value with
        | _ when String.Equals(value, nameof (ToppingAmount.Light)) -> Result.Ok ToppingAmount.Light
        | _ when String.Equals(value, nameof (ToppingAmount.Medium)) -> Result.Ok ToppingAmount.Medium
        | _ when String.Equals(value, nameof (ToppingAmount.Extra)) -> Result.Ok ToppingAmount.Extra
        | _ -> Result.Error $"'{value}' is not a valid topping amount."

    static member toString amount =
        match amount with
        | ToppingAmount.Light -> "Light"
        | ToppingAmount.Medium -> "Medium"
        | ToppingAmount.Extra -> "Extra"

type PizzaSize =
    | Small
    | Medium
    | Large

    static member tryFromString value =
        match value with
        | _ when String.Equals(value, nameof (PizzaSize.Small)) -> Result.Ok PizzaSize.Small
        | _ when String.Equals(value, nameof (PizzaSize.Medium)) -> Result.Ok PizzaSize.Medium
        | _ when String.Equals(value, nameof (PizzaSize.Large)) -> Result.Ok PizzaSize.Large
        | _ -> Result.Error $"'{value}' is not a valid pizza size."

    static member toString size =
        match size with
        | PizzaSize.Small -> "Small"
        | PizzaSize.Medium -> "Medium"
        | PizzaSize.Large -> "Large"

type Pizza =
    { Size: PizzaSize
      Toppings: (Topping * ToppingAmount) list }

type PickupTime = PickupTime of DateTimeOffset

type Order =
    { Pizzas: Pizza list
      PickupTime: PickupTime }
