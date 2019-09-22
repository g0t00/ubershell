open Revery;
open Unix;
open Revery.Math;
open Revery.UI;
open Revery.UI.Components;
open Sys;
/* open Async.Std; */


  type state = {
    procIn: option(out_channel),
    running: bool,
    text: string
  };
  type action =
    | SetRunning(bool)
    | SetProcIn(Pervasives.out_channel)
    | AddString(string);

  let reducer = (a, state) =>
    switch (a) {
      | SetRunning(v) => {...state, running: v}
      | SetProcIn(v) => {...state, procIn: Some(v)}
      | AddString(v) => {...state, text: state.text ++ v}
      };
module Terminal = {
  let component = React.component("Terminal");


  let createElement = (~children, ()) => component(hooks => {
      let (state, dispatch, hooks) = Hooks.reducer(~initialState={procIn: None, running: false, text: ""}, reducer, hooks);
      if (state.running === false) {
        let (stdout, stdin) = Unix.open_process("zsh");
        Unix.set_nonblock(Unix.descr_of_in_channel(stdout));
        print_endline("setup");
        /* output_string(stdin, "echo asd\nsleep 3\necho yolo\n"); */
        /* flush(stdin); */
        dispatch(SetProcIn(stdin));
          /* print_endline("test: " ++ input_line(cmd)) */
        let _ = Tick.interval(t => {
          /* print_endline("tick") */
            switch (input_char(stdout)) {
            | text =>
            print_endline("read: " ++ String.make(1, text))
              dispatch(AddString(String.make(1, text)))
            | exception End_of_file => print_endline("EOF")
            | exception Sys_blocked_io => ()
            };
            }, Seconds(0.));
        dispatch(SetRunning(true));
      }
      let textHeaderStyle =
      Style.[
      color(Colors.white),
      fontFamily("Roboto-Regular.ttf"),
      fontSize(24),
      ];
      let handleKeyDown = (evt: NodeEvents.keyEventParams) =>
        switch (state.procIn) {
          | Some(procIn) => {
            let char = switch(evt.key) {
              | Key.KEY_ENTER => "\n"
              | _ => Key.toString(evt.key)
              };
            output_string(procIn, char);
            flush(procIn);
            dispatch(AddString(char));
          };
          | None => print_endline("procin not set")
          };
      let renderLine = line => <Text style=textHeaderStyle text=line/>;
      let lines = Str.split(Str.regexp("\n"), state.text);
      let lines = List.map(renderLine, lines);
      /* let lines = [<Text style=textHeaderStyle text="asd" />]; */
      (hooks,
       <View ref={r => Focus.focus(r)} style=Style.[position(`Absolute), width(500), height(500), color(Colors.red), border(~color=Colors.red, ~width=5)] onKeyDown=handleKeyDown>
         <View>...lines </View>
       </View>)



    });
};
let init = app => {
  let _ = Revery.Log.listen((_, msg) => print_endline("LOG: " ++ msg));

  let win = App.createWindow(app, "Welcome to Revery!");

  /* let test2 = input_line(cat); */

  let containerStyle =
    Style.[
      position(`Absolute),
      justifyContent(`Center),
      alignItems(`Center),
      bottom(0),
      top(0),
      left(0),
      right(0),
    ];

  let innerStyle = Style.[flexDirection(`Row), alignItems(`FlexEnd)];
  let element =
    <View  style=containerStyle  >
      <View style=innerStyle >
        /* <animatedText delay=0.0 textContent="Welcome" /> */
        /* <animatedText delay=0.5 textContent="to" /> */
        /* <animatedText delay=1. textContent="Revery" /> */
        <Terminal  />
      </View>
      /* <Text text="aaa" /> */
    </View>;
  let _ = UI.start(win, element);
  ();
};

App.start(init);
