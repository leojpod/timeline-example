import './__built.css'
import { Elm } from './elm/Main.elm'

Elm.Main.init({ flags: { millisecNow: new Date().getTime() } })
