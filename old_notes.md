# Unrelated old readme
## Basic Reason Template

Hello! This project allows you to quickly get started with Reason and BuckleScript. If you wanted a more sophisticated version, try the `react` template (`bsb -theme react -init .`).

Build
```
npm run build
```

Build + Watch
```
npm run start
```
Editor: If you use `vscode`, Press `Windows + Shift + B` it will build automatically

## Add to web
```
OptionsPanel.re
```
Find Node.div, add
```
          Node.button(
            [
              Attr.on_click(_ => {
                Printf.printf(
                  "%s\n%!",
                  Extraction_uhexp.extraction_call(
                    ~t=model |> Model.get_program |> Program.get_uhexp,
                  ),
                );
                Event.Ignore;
              }),
            ],
            [Node.text("Extraction to Ocaml")],
          ),
```