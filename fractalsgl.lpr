program FractalsGL;

{$mode delphi}

uses
  Codebot.System,
  Interfaces,
  Forms, LazOpenGLContext, Main, FractalScene;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TFractalForm, FractalForm);
  Application.Run;
end.
