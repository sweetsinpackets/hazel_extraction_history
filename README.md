# Note

This is a repo only for backup. The code is the extraction for EECS590 project, which transfer ```UHExp.t``` to ocaml.

The base is hazel in old version, so maybe new environment will fail to make. Maybe prepare a new opam is great.

The compiled file is copied to ```www``` folder, manually open the index. The version of hazel still supports gradual type (different output type for case).

Note that the version has some problems. Probably lies in bad ```UNK``` pass check and ```Letline```, because a block's type is only decided by the expression line.

# Unrelated old readme
## Basic Reason Template

Hello! This project allows you to quickly get started with Reason and BuckleScript. If you wanted a more sophisticated version, try the `react` template (`bsb -theme react -init .`).

## Build
```
npm run build
```

## Build + Watch

```
npm run start
```


## Editor
If you use `vscode`, Press `Windows + Shift + B` it will build automatically

## add to web
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