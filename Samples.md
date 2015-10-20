# F# Samples #

```fs

module Tests
open global.EventHelix.Frappuccino.FRP
open global.EventHelix.Frappuccino.Game.Animation2D
open global.EventHelix.Frappuccino.Game.Input
open Microsoft.Xna.Framework.Input
open SI
open EventHelix.Frappuccino.Units.Measure
open EventHelix.Frappuccino.Game.Audio
open EventHelix.Frappuccino.Game.UI.Menus
open EventHelix.Frappuccino.Game.Animation2D.Operators
open EventHelix.Frappuccino.Util.Xna



let bear =
@"C:\Users\Saagar\Pictures\gummy.jpg"
|> Animation.fromFile
|> Animation.elongate 0.3f
let bullet = Animation.rectangle 10.0f 20.0f
let rec minigame() =
let col = Signal.untilWhile ( Keyboard.pressed Keys.Enter -=> Color.white ) Color.black
let x = Signal.holdWhile 0.0<_> (Keyboard.pressed Keys.Left -=> -10.<_> .|. Keyboard.pressed Keys.Right -=> 10.<_>)
let y = Signal.holdWhile 0.0<_> (Keyboard.pressed Keys.Up -=> -10.<_> .|. Keyboard.pressed Keys.Down -=> 10.<_>)
let rec game() =


let enemy1 =
bear
|> moveX (wigglef * 10.f)
|> shiftY 100Z
let enemy2 =
bear
|> moveX (wagglef * 10.f)
|> shiftY 300Z
|> color col
let newPlayer() =
let x = Signal.holdWhile 0.0f (Keyboard.pressed Keys.Left -=> -10.f .|. Keyboard.pressed Keys.Right -=> 10.f)
let y = Signal.holdWhile 0.0f (Keyboard.pressed Keys.Up -=> -10.f .|. Keyboard.pressed Keys.Down -=> 10.f)
bear
|> moveXY x y
|> Animation.keepWithin (bounds())

let enemies = enemy1 + enemy2 |> keepWithin2(bounds())
let player = Signal.switchReduceWithSelf (newPlayer()) (fun self -> when'(Animation.collidesWith self enemies) -=> (fun _ -> newPlayer()))
let enemy3 =
bear
|> Animation.chase(Animation.position player)
let lives = Signal.accumulate 5 (when' (player |>collidesWith2 enemies) -=> ((-)$1))
Signal.iter(printfn"%d") lives
let gameOver =
@"C:\Users\Saagar\Pictures\249-artwork-focus.jpg"
|> Animation.fromFile
|> Signal.until(Keyboard.pressed Keys.Escape -=> Animation.exit)

let sprites = player + enemies + enemy3
sprites.Until(when'(lives <=* Signal.constant 0) -=> gameOver .|. (Keyboard.pressed Keys.Escape -=> Animation.exit)) //|> Animation.withSound (Sound.fromFile @"C:\Users\Public\Music\Sample Music\Kalimba.mp3")
Animation.run (game())
let `Test of Signal.delay`(distanceBetweenAnimationsInTime) =
let mouseFollower = bear |> shift2 Mouse.position
List.init 10 (fun i -> Signal.delay (System.TimeSpan.FromMilliseconds(float i * distanceBetweenAnimationsInTime)) mouseFollower)
|> List.reduce union
|> Animation.run
let `Test of Signal.delay + keyboard user input`(distanceBetweenAnimationsInTime) =
let col = Signal.untilWhile ( Keyboard.pressed Keys.Enter -=> Color.white ) Color.black
let x = Signal.holdWhile 0.0<_> (Keyboard.pressed Keys.Left -=> -10.<_> .|. Keyboard.pressed Keys.Right -=> 10.<_>)
let y = Signal.holdWhile 0.0<_> (Keyboard.pressed Keys.Up -=> -10.<_> .|. Keyboard.pressed Keys.Down -=> 10.<_>)
let mouseFollower = bear |> Animation.moveXY (Float.Frame.integral (x |> box |> unbox<float<pixel/frame> signal>)) (Float.Frame.integral (y |> box |> unbox<float<pixel/frame> signal>))
List.init 10 (fun i -> Signal.delay (System.TimeSpan.FromMilliseconds(float i * distanceBetweenAnimationsInTime)) mouseFollower)
|> List.reduce Animation.append
|> Animation.run
let `Test of Float.Frame.integral + keyboard user input + Animation.moveXY`() =
let x = Signal.holdWhile 0.0<_> (Keyboard.pressed Keys.Left -=> -10.<pixel/frame^2> .|. Keyboard.pressed Keys.Right -=> 10.<_>)
let y = Signal.holdWhile 0.0<_> (Keyboard.pressed Keys.Up -=> -10.<pixel/frame^2> .|. Keyboard.pressed Keys.Down -=> 10.<_>)

bear
|> Animation.moveXY (x |> Float.Frame.integral |> Float.Frame.integral) (y |> Float.Frame.integral |> Float.Frame.integral)
//|> Animation.keepWithin (bounds())
|> Animation.run
//`Test of Signal.delay + keyboard user input`(150.)
//`Test of Signal.delay`(100.)
let scrollerShooter() =
let x = Signal.holdWhile 0.0f (Keyboard.pressed Keys.Left -=> -10.f .|. Keyboard.pressed Keys.Right -=> 10.f)
let y = Signal.holdWhile 0.0f (Keyboard.pressed Keys.Up -=> -10.f .|. Keyboard.pressed Keys.Down -=> 10.f)

let crossHair =
@"C:\Users\Saagar\Pictures\crosshairs.png"
|> Animation.fromFile
|> shift2 Mouse.position
|> shiftXY 20Z 20Z // shift has a bug
|> Animation.elongate 0.2f
let player =
bear
|> moveXY x y
let game = player + crossHair
Animation.run game
//`Test of Signal.delay`(300.)
//minigame()
let `menu test`() =
let col = Signal.untilWhile ( Keyboard.pressed Keys.Enter -=> Color.white ) Color.black
let x = Signal.holdWhile 0.0<_> (Keyboard.pressed Keys.Left -=> -10.<_> .|. Keyboard.pressed Keys.Right -=> 10.<_>)
let y = Signal.holdWhile 0.0<_> (Keyboard.pressed Keys.Up -=> -10.<_> .|. Keyboard.pressed Keys.Down -=> 10.<_>)
let btn =
bear
|> Animation.stretch(abs wigglef)
let btn2 =
bear
|> Animation.expand 10.0f 7.0f
|> Animation.shift(Signal.constant (new Microsoft.Xna.Framework.Vector2(100.f,100.f)))
|> Animation.withColor Microsoft.Xna.Framework.Color.Blue
let player =
bear
|> Animation.moveXY x y
let other =
bear
|> moveX (wigglef * 10.f)
|> Animation.stretch (abs wagglef)
let background =
//Animation.rectangle 200 200
Animation.solid
|> Animation.color Color.black
//|> Animation.moveX (wiggle*Signal.constant(10.<_>))
let menu = Menu.create background [btn => player; btn2 => other]
Animation.run menu
let `When setting position to mouse position the animation should follow the mouse`() =
bear
|> shift2 Mouse.position
|> Animation.run
let `shape test`() =
//    let col = Signal.untilWhile ( Keyboard.pressed Keys.Enter -=> Color.white ) Color.black
//    let x = Signal.holdWhile 0.0<_> (Keyboard.pressed Keys.Left -=> -10.<_> .|. Keyboard.pressed Keys.Right -=> 10.<_>)
//    let y = Signal.holdWhile 0.0<_> (Keyboard.pressed Keys.Up -=> -10.<_> .|. Keyboard.pressed Keys.Down -=> 10.<_>)
300
|> Animation.circle
|> color Color.red
|> moveX wigglef
|> stretch (abs wigglef)
|> Animation.run
let `polar test`() =
let angle = Signal.accumulate 0.0f<_> (Keyboard.pressed Keys.Left -=> (fun x -> x - 10.0f<degree>)  .|. Keyboard.pressed Keys.Right -=> (fun x -> x + 10.0f<degree>)) |>* Radians.ofDegrees
let magnitude = Signal.holdWhile 0.0f (Keyboard.pressed Keys.Up -=> -10.0f .|. Keyboard.pressed Keys.Down -=> 10.0f)
bear
|> move2 (Vector2S.Polar(angle, magnitude))
|> rotate2 angle
|> Animation.run
let bullets() =
let x = 0.0f.HoldWhile(Keyboard.pressed Keys.Left -=> -10.0f .|. Keyboard.pressed Keys.Right -=> 10.0f)
let y = 0.0f.HoldWhile(Keyboard.pressed Keys.Up -=> -10.0f .|. Keyboard.pressed Keys.Down -=> 10.0f)
let player =
bear
|> moveXY x y
let newBullet () =
let mouse = Mouse.position.Value
let player = position(player).Value
let angle = player.AngleFrom mouse
bullet
|> color Color.red
|> move2 (Vector2S.Polar(angle, 10.0f))

let bullets =
Signal.switchGather(fun anims -> Mouse.leftButton.Pressed ==> fun _ -> anims + (newBullet())) Animation.empty
Animation.run(player + bullets)
let `keep within should ensure the animation stays within the bounds`() =
let x = 0.0f.HoldWhile(Keyboard.pressed Keys.Left -=> -10.0f .|. Keyboard.pressed Keys.Right -=> 10.0f)
bear
|> moveX x
|> keepWithin2 (bounds())
|> Animation.run
let followtest() =
bear
|> follow2 Mouse.position
|> run
//`keep within should ensure the animation stays within the bounds`()
followtest()
```