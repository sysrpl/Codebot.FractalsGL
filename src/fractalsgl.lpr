program FractalsGL;

{$mode delphi}

uses
  Codebot.System,
  Codebot.Render.Contexts,
  Interfaces,
  Forms, Dialogs, LazOpenGLContext, Main, FractalScene;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TFractalForm, FractalForm);
  Application.Run;
end.
