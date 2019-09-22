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
    text: string,
    escapeCode: bool
  };
  type action =
    | SetRunning(bool)
    | SetProcIn(Pervasives.out_channel)
    | AddString(string);

  let reducer = (a, state) =>
    switch (a) {
      | SetRunning(v) => {...state, running: v}
      | SetProcIn(v) => {...state, procIn: Some(v)}
      | AddString(v) => {
        switch(state.escapeCode) {
          | false =>
            v.[0] == '\x1B' ? {... state, escapeCode: true} : {...state, text: state.text ++ v}

          | true => {
            let code = Char.code(String.get(v, 0));
            print_endline("code: " ++ string_of_int(code));
            code >= 0x40 && code <= 0x7E ? {...state, escapeCode: false} : state;
          }
          };
      }
      };
module Terminal = {
  let component = React.component("Terminal");


  let createElement = (~children, ()) => component(hooks => {
      let (state, dispatch, hooks) = Hooks.reducer(~initialState={procIn: None, running: false, text: "", escapeCode: false}, reducer, hooks);
      if (state.running === false) {
        let  (fstdout, procStdout) = Unix.pipe();
        let (fstderr, procStderr) = Unix.pipe();
        let (procStdin, fstdin) = Unix.pipe();
        let _ = Unix.create_process("/bin/zsh", [|"-i", "-i"|], procStdin, procStdout, procStderr);
        /* let (stdout, stdin, stderr) = Unix.open_process_full("/bin/sh", [|"-i"|]); */
        let stdout = Unix.in_channel_of_descr(fstdout);
        let stderr = Unix.in_channel_of_descr(fstderr);
        let stdin = Unix.out_channel_of_descr(fstdin);
        Unix.set_nonblock(fstdout);
        Unix.set_nonblock(fstderr);
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
            switch (input_char(stderr)) {
            | text =>
            print_endline("reade: " ++ String.make(1, text))
              /* dispatch(AddString(String.make(1, text))) */
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
      let handleKeyPress = (evt: NodeEvents.keyPressEventParams) =>
        switch (state.procIn) {
          | Some(procIn) => {
            output_string(procIn, evt.character);
            flush(procIn);
            print_endline("press: " ++ evt.character);
          }
          | _ => ()
          };
      let handleKeyDown = (evt: NodeEvents.keyEventParams) =>
        switch (state.procIn) {
          | Some(procIn) => {
              let char: option(string) = switch(evt.key) {
              | Key.KEY_ENTER => Some("\n")
              | Key.KEY_TAB => Some("\t")
              | Key.KEY_LEFT => Some("\x1B[D")
              | Key.KEY_C =>
                evt.ctrlKey ? Some("\x03") : None;
              | _ => None
              };
            switch (char) {
              | Some(char) => {
            output_string(procIn, char);
            flush(procIn);
            print_endline("write: " ++ char);
              }
              | _ => ()
              };
          };
          | None => print_endline("procin not set")
          };
      let renderLine = line => <Text style=textHeaderStyle text=line/>;
      let lines = Str.split(Str.regexp("\n"), state.text);
      let lines = List.map(renderLine, lines);
      /* let lines = [<Text style=textHeaderStyle text="asd" />]; */
      (hooks,
       <View ref={r => Focus.focus(r)} style=Style.[flexGrow(1), color(Colors.red), border(~color=Colors.red, ~width=5)] onKeyDown=handleKeyDown onKeyPress=handleKeyPress>
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
      justifyContent(`Center),
      alignItems(`Stretch),
      flexGrow(1)
    ];

  let innerStyle = Style.[flexDirection(`Row), alignItems(`FlexEnd)];
  let element =
      <View style=innerStyle >
        /* <animatedText delay=0.0 textContent="Welcome" /> */
        /* <animatedText delay=0.5 textContent="to" /> */
        /* <animatedText delay=1. textContent="Revery" /> */
        <Terminal  />
      /* <Text text="aaa" /> */
    </View>;
  let _ = UI.start(win, element);
  ();
};

App.start(init);
