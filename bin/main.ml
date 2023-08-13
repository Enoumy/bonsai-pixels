open! Core
open! Bonsai_web
module Form = Bonsai_web_ui_form

module Coordinate = struct
  module T = struct
    type t =
      { y : int
      ; x : int
      }
    [@@deriving sexp, compare]
  end

  include T
  include Comparable.Make (T)
end

module Resolution = struct
  type t =
    { height : int
    ; width : int
    }
  [@@deriving sexp, compare, typed_fields]

  let is_coordinate_within t ~coordinate:{ Coordinate.y; x } =
    x >= 0 && x < t.width && y >= 0 && y < t.height
  ;;

  let field_form ~placeholder =
    let open Bonsai.Let_syntax in
    let%sub form = Form.Elements.Textbox.int ~placeholder () in
    Form.Dynamic.with_default (Value.return 50) form
  ;;

  let form_for_field : type a. a Typed_field.t -> a Form.t Computation.t
    = function
    | Height -> field_form ~placeholder:"height"
    | Width -> field_form ~placeholder:"width"
  ;;

  let label_for_field = `Inferred
end

let resolution_form = Form.Typed.Record.make (module Resolution)
let tomato_color = `Hex "#ff6347"

let color_picker : [ `Hex of string ] Form.t Computation.t =
  let open Bonsai.Let_syntax in
  let%sub form = Form.Elements.Color_picker.hex () in
  Form.Dynamic.with_default (Value.return tomato_color) form
;;

module Style =
  [%css
  stylesheet
    {|
.pixel-grid {
  display: grid;
  grid-template-columns: repeat(var(--width), 1rem);
  grid-template-rows: repeat(var(--height), 1rem);
}

.pixel {
  width: 100%;
  height: 100%;
}
    |}]

let pixel_canvas
  ~(resolution : Resolution.t Value.t)
  ~(brush_color : Css_gen.Color.t Value.t)
  : Vdom.Node.t Computation.t
  =
  let open Bonsai.Let_syntax in
  let%sub background_layer =
    let%arr { width; height } = resolution in
    Coordinate.Map.of_alist_exn
    @@
    let%bind.List y = List.init height ~f:Fn.id in
    let%map.List x = List.init width ~f:Fn.id in
    { Coordinate.x; y }, (`Name "white" : Css_gen.Color.t)
  in
  let%sub pixels_drawn_by_user, draw =
    Bonsai.state_machine0
      ()
      ~default_model:Coordinate.Map.empty
      ~apply_action:(fun _ prev (coordinate, color) ->
        Map.set prev ~key:coordinate ~data:color)
  in
  let%sub pixels_to_draw =
    let%sub pixels_drawn_by_user =
      (* NOTE: Incr.compute is used so that we can filter user pixels based
         on the currently selected resolution. *)
      Bonsai.Incr.compute
        (Value.both pixels_drawn_by_user resolution)
        ~f:(fun input ->
          let open Incremental.Let_syntax in
          let%pattern_bind pixels, resolution = input in
          Incr_map.filter_mapi' pixels ~f:(fun ~key ~data ->
            let%map resolution = resolution
            and data = data in
            match
              Resolution.is_coordinate_within resolution ~coordinate:key
            with
            | true -> Some data
            | false -> None))
    in
    Bonsai.Map.merge background_layer pixels_drawn_by_user ~f:(fun ~key:_ ->
      function
      | `Left color | `Right color -> Some color
      | `Both (_background_color, user_color) -> Some user_color)
  in
  let%sub brush_color = Bonsai.yoink brush_color in
  let%sub pixels_to_draw =
    Bonsai.assoc
      (module Coordinate)
      pixels_to_draw
      ~f:(fun coordinate color ->
        let%arr coordinate = coordinate
        and color = color
        and draw = draw
        and brush_color = brush_color in
        Vdom.Node.div
          ~attrs:
            [ Style.pixel
            ; Vdom.Attr.style (Css_gen.background_color color)
            ; Vdom.Attr.on_mouseenter (fun _ ->
                let%bind.Effect brush_color = brush_color in
                let brush_color =
                  match brush_color with
                  | Bonsai.Computation_status.Inactive -> tomato_color
                  | Active x -> x
                in
                draw (coordinate, brush_color))
            ]
          [])
  in
  let%arr pixels_to_draw = pixels_to_draw
  and { width; height } = resolution in
  let attrs =
    [ Style.pixel_grid
    ; Style.Variables.set
        ~width:(Int.to_string width)
        ~height:(Int.to_string height)
        ()
    ]
  in
  match `Vdom_node_with_map_children with
  | `Map_dot_data -> Vdom.Node.div ~attrs (Map.data pixels_to_draw)
  | `Vdom_node_with_map_children ->
    Vdom_node_with_map_children.make
      ~tag:"div"
      ~attr:(Vdom.Attr.many attrs)
      pixels_to_draw
;;

let component =
  let open Bonsai.Let_syntax in
  let%sub resolution_form = resolution_form in
  let%sub color_picker_form = color_picker in
  let%sub resolution_form_view =
    let%arr resolution_form = resolution_form in
    Form.view_as_vdom resolution_form
  in
  let%sub color_picker_view =
    let%arr color_picker_form = color_picker_form in
    Form.view_as_vdom color_picker_form
  in
  let%sub pixel_canvas =
    let%sub resolution =
      let%arr resolution_form = resolution_form in
      Form.value resolution_form
    in
    match%sub resolution with
    | Error error ->
      let%arr error = error in
      Vdom.Node.textf
        "Please pick a valid resolution. Error: %s"
        (Error.to_string_hum error)
    | Ok resolution ->
      let%sub brush_color =
        let%arr color_picker_form = color_picker_form in
        let (`Hex color) =
          Form.value_or_default ~default:tomato_color color_picker_form
        in
        (`Hex color : Css_gen.Color.t)
      in
      pixel_canvas ~resolution ~brush_color
  in
  let%arr resolution_form_view = resolution_form_view
  and color_picker_view = color_picker_view
  and pixel_canvas = pixel_canvas in
  View.vbox
    ~cross_axis_alignment:Center
    [ Vdom.Node.h1 [ View.text "Bonsai Pixels" ]
    ; View.hbox [ resolution_form_view; color_picker_view ]
    ; pixel_canvas
    ]
;;

let app =
  View.Theme.set_for_app
    (Value.return (Kado.theme ~style:Dark ~version:Bleeding ()))
    component
;;

let () = Bonsai_web.Start.start app
