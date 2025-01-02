unit MistralAI.Tutorial.FMX;

{ Tutorial Support Unit

   WARNING:
     This module is intended solely to illustrate the examples provided in the
     README.md file of the repository :
          https://github.com/MaxiDonkey/DelphiMistralAI
     Under no circumstances should the methods described below be used outside
     of the examples presented on the repository's page.
}

interface

uses
  System.SysUtils, System.Classes, Winapi.Messages, FMX.Types, FMX.StdCtrls, FMX.ExtCtrls,
  FMX.Controls, FMX.Forms, Winapi.Windows, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.Media, FMX.Objects, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, System.UITypes,
  System.Types,
  MistralAI, MistralAI.Types, MistralAI.Functions.Core;

type
  TToolProc = procedure (const Value: TCalledFunction; Func: IFunctionCore) of object;

  /// <summary>
  /// Represents a tutorial hub for handling visual components in a Delphi application,
  /// including text display, button interactions, and navigation through pages.
  /// </summary>
  TFMXTutorialHub = class
  private
    FMemo1: TMemo;
    FMemo2: TMemo;
    FButton: TButton;
    FFileId: string;
    FJobId: string;
    FBatchId: string;
    FFileName: string;
    FTool: IFunctionCore;
    FToolCall: TToolProc;
    FPage: Integer;
    FFileOverride: Boolean;
    FCancel: Boolean;
    procedure OnButtonClick(Sender: TObject);
    procedure SetButton(const Value: TButton);
    procedure SetMemo1(const Value: TMemo);
    procedure SetMemo2(const Value: TMemo);
  public
    /// <summary>
    /// Advances the tutorial to the next page.
    /// </summary>
    procedure NextPage;
    /// <summary>
    /// Gets or sets the first memo component for displaying messages or data.
    /// </summary>
    property Memo1: TMemo read FMemo1 write SetMemo1;
    /// <summary>
    /// Gets or sets the second memo component for displaying additional messages or data.
    /// </summary>
    property Memo2: TMemo read FMemo2 write SetMemo2;
    /// <summary>
    /// Gets or sets the button component used to trigger actions or handle cancellation.
    /// </summary>
    property Button: TButton read FButton write SetButton;
    /// <summary>
    /// Gets or sets a value indicating whether the operation has been canceled.
    /// </summary>
    property Cancel: Boolean read FCancel write FCancel;
    /// <summary>
    /// Gets or sets the file identifier associated with the tutorial hub.
    /// </summary>
    property FileId: string read FFileId write FFileId;
    /// <summary>
    /// Gets or sets the job identifier associated with the tutorial hub.
    /// </summary>
    property JobId: string read FJobId write FJobId;
    /// <summary>
    /// Gets or sets the batch identifier associated with the tutorial hub.
    /// </summary>
    property BatchId: string read FBatchId write FBatchId;
    /// <summary>
    /// Gets or sets the name of the file associated with the tutorial hub.
    /// </summary>
    property FileName: string read FFileName write FFileName;
    /// <summary>
    /// Gets or sets the core function tool used for processing.
    /// </summary>
    property Tool: IFunctionCore read FTool write FTool;
    /// <summary>
    /// Gets or sets the procedure for handling tool-specific calls.
    /// </summary>
    property ToolCall: TToolProc read FToolCall write FToolCall;
    /// <summary>
    /// Gets or sets the current page number within the tutorial.
    /// </summary>
    property Page: Integer read FPage write FPage;
    /// <summary>
    /// Gets or sets a value indicating whether file overrides are allowed.
    /// </summary>
    property FileOverride: Boolean read FFileOverride write FFileOverride;
    constructor Create(const AMemo1, AMemo2: TMemo; const AButton: TButton);
  end;

  procedure Cancellation(Sender: TObject);
  function DoCancellation: Boolean;
  procedure Start(Sender: TObject);

  procedure Display(Sender: TObject); overload;
  procedure Display(Sender: TObject; Value: string); overload;
  procedure Display(Sender: TObject; Value: TArray<string>); overload;
  procedure Display(Sender: TObject; Value: TChat); overload;
  procedure Display(Sender: TObject; Value: TCodestral); overload;
  procedure Display(Sender: TObject; Value: TEmbeddings); overload;
  procedure Display(Sender: TObject; Value: TModel); overload;
  procedure Display(Sender: TObject; Value: TModels); overload;
  procedure Display(Sender: TObject; Value: TModerationResult); overload;
  procedure DisplayEx(Sender: TObject; Value: TModerationResult); overload;
  procedure Display(Sender: TObject; Value: TModeration); overload;
  procedure DisplayEx(Sender: TObject; Value: TModeration); overload;
  procedure Display(Sender: TObject; Value: TFile); overload;
  procedure Display(Sender: TObject; Value: TFiles); overload;
  procedure Display(Sender: TObject; Value: TDeletedResult); overload;
  procedure Display(Sender: TObject; Value: TDownLoadFile); overload;
  procedure Display(Sender: TObject; Value: TJobOut); overload;
  procedure Display(Sender: TObject; Value: TJobOutProgress); overload;
  procedure Display(Sender: TObject; Value: TListFineTuningJobs); overload;
  procedure Display(Sender: TObject; Value: TBatchJob); overload;
  procedure Display(Sender: TObject; Value: TBatchJobList); overload;

  procedure DisplayStream(Sender: TObject; Value: string); overload;
  procedure DisplayStream(Sender: TObject; Value: TChat); overload;
  procedure DisplayStream(Sender: TObject; Value: TCodestral); overload;

  function F(const Name, Value: string): string; overload;
  function F(const Name: string; const Value: TArray<string>): string; overload;
  function F(const Name: string; const Value: boolean): string; overload;
  function F(const Name: string; const State: Boolean; const Value: Double): string; overload;

  function CodeBefore: string;
  function CodeAfter: string;

var
  /// <summary>
  /// A global instance of the <see cref="TVCLTutorialHub"/> class used as the main tutorial hub.
  /// </summary>
  /// <remarks>
  /// This variable serves as the central hub for managing tutorial components, such as memos, buttons, and pages.
  /// It is initialized dynamically during the application's runtime, and its memory is automatically released during
  /// the application's finalization phase.
  /// </remarks>
  TutorialHub: TFMXTutorialHub = nil;

implementation

uses
  System.DateUtils;

function UnixIntToDateTime(const Value: Int64): TDateTime;
begin
  Result := TTimeZone.Local.ToLocalTime(UnixToDateTime(Value));
end;

function UnixDateTimeToString(const Value: Int64): string;
begin
  Result := DateTimeToStr(UnixIntToDateTime(Value))
end;

procedure Cancellation(Sender: TObject);
begin
  Display(Sender, 'The operation was cancelled' + sLineBreak);
  TutorialHub.Cancel := False;
end;

function DoCancellation: Boolean;
begin
  Result := TutorialHub.Cancel;
end;

procedure Start(Sender: TObject);
begin
  Display(Sender, 'Please wait...');
  Display(Sender);
end;

procedure Display(Sender: TObject; Value: string);
var
  M: TMemo;
begin
  if Sender is TMemo then
    M := Sender as TMemo else
    M := (Sender as TFMXTutorialHub).Memo1;
  M.Lines.Add(Value);
  M.ViewportPosition := PointF(M.ViewportPosition.X, M.Content.Height - M.Height);
end;

procedure Display(Sender: TObject; Value: TArray<string>);
begin
  var index := 0;
  for var Item in Value do
    begin
      if not Item.IsEmpty then
        begin
          if index = 0 then
            Display(Sender, Item) else
            Display(Sender, '    ' + Item);
        end;
      Inc(index);
    end;
end;

procedure Display(Sender: TObject);
begin
  Display(Sender, sLineBreak);
end;

procedure Display(Sender: TObject; Value: TChat);
begin
  for var Item in Value.Choices do
    if Item.FinishReason = TFinishReason.tool_calls then
      begin
        if Assigned(TutorialHub.ToolCall) then
          TutorialHub.ToolCall(Item.Message.ToolsCalls[0], TutorialHub.Tool);
      end
    else
      begin
        Display(Sender, Item.Message.Content);
      end;
end;

procedure Display(Sender: TObject; Value: TCodestral); overload;
begin
  for var Item in Value.Choices do
    if Item.FinishReason = TCodestralFinishReason.cstool_calls then
      begin
        if Assigned(TutorialHub.ToolCall) then
          TutorialHub.ToolCall(Item.Message.ToolsCalls[0], TutorialHub.Tool);
      end
    else
      begin
        Display(Sender, Item.Message.Content);
      end;
end;

procedure Display(Sender: TObject; Value: TEmbeddings); overload;
begin
  var i := 0;
  for var Item in Value.Data do
    begin
      var j := 0;
      for var SubItem in Item.Embedding do
          begin
            Display(Sender, Format('Vector[%d, %d] = %s', [i, j, SubItem.ToString(ffNumber, 8, 8)]));
            Inc(j);
            Application.ProcessMessages;
          end;
      Display(Sender);
      Inc(i);
    end;
end;

procedure Display(Sender: TObject; Value: TModel);
begin
  Display(Sender, [
    Value.Id,
    F('Name', Value.Name),
    F('Description', Value.Description),
    F('Deprecation', Value.Deprecation),
    F('Default temperature', Value.DefaultModelTemperature.ToString(ffNumber, 3, 2)),
    F('Completion chat', BoolToStr(Value.Capabilities.CompletionChat, True)),
    F('Completion fim', BoolToStr(Value.Capabilities.CompletionFim, True)),
    F('Function calling', BoolToStr(Value.Capabilities.FunctionCalling, True)),
    F('Fine-tuning', BoolToStr(Value.Capabilities.FineTuning, True)),
    F('Vision', BoolToStr(Value.Capabilities.Vision, True))
  ]);
  Display(Sender, EmptyStr);
end;

procedure Display(Sender: TObject; Value: TModels);
begin
  if Length(Value.Data) = 0 then
    begin
      Display(Sender, 'No model found');
      Exit;
    end;
  for var Item in Value.Data do
    begin
      Display(Sender, Item);
      Application.ProcessMessages;
    end;
  Display(Sender);
end;

procedure DisplayEx(Sender: TObject; Value: TModerationResult);
var
  Temp: TArray<string>;
begin
  if Value.Warning then
    begin
      Temp := [''];
      for var Item in Value.Warnings do
        Temp := Temp + [F(Item.Category, (Item.Score * 100).ToString(ffNumber, 3, 2)) + '%'];
      Display(Sender, Temp);
      Display(Sender);
    end
  else
    begin
      Display(Sender, 'No moderation is necessary');
    end;
end;

procedure Display(Sender: TObject; Value: TModerationResult);
begin
  if Value.Warning then
    begin
      Display(Sender, [EmptyStr,
        F(Classifiers[0], Value.Categories.Sexual, Value.Scores.Sexual),
        F(Classifiers[1], Value.Categories.Hate_and_discrimination, Value.Scores.Hate_and_discrimination),
        F(Classifiers[2], Value.Categories.Violence_and_threats, Value.Scores.Violence_and_threats),
        F(Classifiers[3], Value.Categories.Dangerous_and_criminal_content, Value.Scores.Dangerous_and_criminal_content),
        F(Classifiers[4], Value.Categories.Selfharm, Value.Scores.Selfharm),
        F(Classifiers[5], Value.Categories.Health, Value.Scores.Health),
        F(Classifiers[6], Value.Categories.Financial, Value.Scores.Financial),
        F(Classifiers[7], Value.Categories.Law, Value.Scores.Law),
        F(Classifiers[8], Value.Categories.Pii, Value.Scores.Pii)
      ]);
      Display(Sender);
    end
  else
    begin
      Display(Sender, 'No moderation is necessary');
    end;
end;

procedure Display(Sender: TObject; Value: TModeration);
begin
  for var Item in Value.Results do
    Display(Sender, Item);
end;

procedure DisplayEx(Sender: TObject; Value: TModeration);
begin
  for var Item in Value.Results do
    DisplayEx(Sender, Item);
end;

procedure Display(Sender: TObject; Value: TFile);
begin
  Display(Sender, [
    F(Value.FileName, Value.Id),
    F('Source', [
      Value.Source.ToString,
      F('Purpose', Value.Purpose.ToString),
      F('Object', Value.&Object),
      F('Bytes', Value.Bytes.ToString),
      F('NumLines', Value.NumLines.ToString)
    ])
  ]);
  Display(Sender);
end;

procedure Display(Sender: TObject; Value: TFiles);
begin
  if Length(Value.Data) = 0 then
    begin
      Display(Sender, 'No file found');
      Exit;
    end;
  for var Item in Value.Data do
    Display(Sender, Item);
end;

procedure Display(Sender: TObject; Value: TDeletedResult);
begin
  Display(Sender, [
    F('Id', Value.Id),
    F('Deleted', [
      BoolToStr(Value.Deleted, True),
      F('Object', Value.&Object)
    ])
  ]);
end;

procedure Display(Sender: TObject; Value: TDownLoadFile);
begin
  if TutorialHub.FileName.IsEmpty then
    raise Exception.Create('No filename defined for download.');
  Value.SaveToFile(TutorialHub.FileName, not TutorialHub.FileOverride);
  Display(Sender, Format('File downloaded and saved as "%s"', [TutorialHub.FileName]));
end;

procedure Display(Sender: TObject; Value: TJobOut);
begin
  Display(Sender, [
    Value.Id,
    F('AutoStart', [
      BoolToStr(Value.AutoStart, True),
      F('Model', Value.Model),
      F('Status', Value.Status.ToString),
      F('FineTuningModel', Value.FineTuningModel)
    ])
  ]);
end;

procedure Display(Sender: TObject; Value: TJobOutProgress);
begin
  Display(Sender, [
    Value.Id,
    F('AutoStart', [
      BoolToStr(Value.AutoStart, True),
      F('Model', Value.Model),
      F('Status', Value.Status.ToString),
      F('FineTuningModel', Value.FineTuningModel)
    ])
  ]);
  for var Item in Value.Events do
    Display(Sender, [
      Item.Name,
      Item.Data.Status.ToString,
      Item.Data.Error,
      UnixDateTimeToString(Item.CreatedAt)
    ]);
end;

procedure Display(Sender: TObject; Value: TListFineTuningJobs);
begin
  Display(Sender, F('Total', [Value.Total.ToString, F('Object', Value.&Object.ToString)]));
  for var Item in Value.Data do
    Display(Sender, F('Id', [
      Item.Id,
      F('Model', Item.Model),
      F('Status', Item.Status.ToString)
    ]));
end;

procedure Display(Sender: TObject; Value: TBatchJob);
begin
  Display(Sender, F('Id', [
    Value.Id,
    F('Model', Value.Model),
    F('Endpoint', Value.Endpoint.ToString),
    F('Status', Value.Status.ToString),
    F('CreatedAt', UnixDateTimeToString(Value.CreatedAt))
  ]));
  if not Value.OutputFile.IsEmpty then
    Display(Sender, F('OutputFile', Value.OutputFile));
  Display(Sender, Value.Metadata);
  Display(Sender);
end;

procedure Display(Sender: TObject; Value: TBatchJobList);
begin
  Display(Sender, F('Total', [
    Value.Total.ToString,
    F('Object', Value.&Object)]));
  for var Item in Value.Data do
    Display(Sender, Item);
end;

procedure DisplayStream(Sender: TObject; Value: string);
var
  M: TMemo;
  CurrentLine: string;
begin
  if Sender is TMemo then
    M := Sender as TMemo
  else
    M := (Sender as TFMXTutorialHub).Memo1;
  var ShouldScroll := M.ViewportPosition.Y >= (M.Content.Height - M.Height - 16);
  M.Lines.BeginUpdate;
  try
    var Lines := Value.Replace(#13, '').Split([#10]);
    if System.Length(Lines) > 0 then
    begin
      if M.Lines.Count > 0 then
        CurrentLine := M.Lines[M.Lines.Count - 1]
      else
        CurrentLine := EmptyStr;
      CurrentLine := CurrentLine + Lines[0];
      if M.Lines.Count > 0 then
        M.Lines[M.Lines.Count - 1] := CurrentLine
      else
        M.Lines.Add(CurrentLine);
      for var i := 1 to High(Lines) do
        M.Lines.Add(Lines[i]);
    end;
  finally
    M.Lines.EndUpdate;
  end;
  if ShouldScroll then
    M.ViewportPosition := PointF(M.ViewportPosition.X, M.Content.Height - M.Height + 1);
end;

procedure DisplayStream(Sender: TObject; Value: TChat);
begin
  if Assigned(Value) then
    DisplayStream(Sender, Value.Choices[0].Delta.Content);
end;

procedure DisplayStream(Sender: TObject; Value: TCodestral);
begin
  if Assigned(Value) then
    DisplayStream(Sender, Value.Choices[0].Delta.Content);
end;

function F(const Name, Value: string): string;
begin
  if not Value.IsEmpty then
    Result := Format('%s: %s', [Name, Value])
end;

function F(const Name: string; const Value: TArray<string>): string;
begin
  var index := 0;
  for var Item in Value do
    begin
      if index = 0 then
        Result := Format('%s: %s', [Name, Item]) else
        Result := Result + '    ' + Item;
      Inc(index);
    end;
end;

function F(const Name: string; const Value: boolean): string;
begin
  Result := Format('%s: %s', [Name, BoolToStr(Value, True)])
end;

function F(const Name: string; const State: Boolean; const Value: Double): string;
begin
  Result := Format('%s (%s): %s%%', [Name, BoolToStr(State, True), (Value * 100).ToString(ffNumber, 3, 3)])
end;

function CodeBefore: string;
begin
  with TStringWriter.Create do
  try
    WriteLine('def is_odd(n):');
    WriteLine('  return n % 2 == 1');
    WriteLine('def test_is_odd():');
    Result := ToString;
  finally
    Free;
  end;
end;

function CodeAfter: string;
begin
  with TStringWriter.Create do
  try
    WriteLine('n = int(input(''Enter a number: ''))');
    WriteLine('print(fibonacci(n))');
    Result := ToString;
  finally
    Free;
  end;
end;

{ TFMXTutorialHub }

constructor TFMXTutorialHub.Create(const AMemo1, AMemo2: TMemo; const AButton: TButton);
begin
  inherited Create;
  Memo1 := AMemo1;
  Memo2 := AMemo2;
  Button := AButton;
  FFileOverride := False;
end;

procedure TFMXTutorialHub.NextPage;
begin
  Inc(FPage);
end;

procedure TFMXTutorialHub.OnButtonClick(Sender: TObject);
begin
  Cancel := True;
end;

procedure TFMXTutorialHub.SetButton(const Value: TButton);
begin
  FButton := Value;
  FButton.OnClick := OnButtonClick;
  FButton.Text := 'Cancel';
end;

procedure TFMXTutorialHub.SetMemo1(const Value: TMemo);
begin
  FMemo1 := Value;
  FMemo1.TextSettings.WordWrap := True;
end;

procedure TFMXTutorialHub.SetMemo2(const Value: TMemo);
begin
  FMemo2 := Value;
  FMemo2.TextSettings.WordWrap := True;
end;

initialization
finalization
  if Assigned(TutorialHub) then
    TutorialHub.Free;
end.
