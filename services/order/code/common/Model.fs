namespace common

open System

type ToppingKind =
    | Pepperoni
    | Cheese
    | Pineapple

    static member tryFromString value =
        match value with
        | _ when String.Equals(value, nameof (ToppingKind.Pepperoni)) -> Result.Ok ToppingKind.Pepperoni
        | _ when String.Equals(value, nameof (ToppingKind.Cheese)) -> Result.Ok ToppingKind.Cheese
        | _ when String.Equals(value, nameof (ToppingKind.Pineapple)) -> Result.Ok ToppingKind.Pineapple
        | _ -> Result.Error $"'{value}' is not a valid topping."

    static member toString topping =
        match topping with
        | ToppingKind.Pepperoni -> "Pepperoni"
        | ToppingKind.Cheese -> "Cheese"
        | ToppingKind.Pineapple -> "Pineapple"

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

type Topping =
    { Kind: ToppingKind
      Amount: ToppingAmount }

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
      Toppings: Topping list }

type PickupTime =
    | PickupTime of DateTimeOffset

    static member toDateTimeOffset(PickupTime value) = value

type OrderId =
    | OrderId of Guid

    static member fromString(value: string) = Guid.Parse(value) |> OrderId

    static member toGuid(OrderId guid) = guid

    static member toString orderId = OrderId.toGuid orderId |> string

type Order =
    { Id: OrderId
      Pizzas: Pizza list
      PickupTime: PickupTime }
