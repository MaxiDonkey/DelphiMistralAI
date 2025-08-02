unit MistralAI.Tutorial.VCL;

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
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  System.UITypes, System.JSON,
  MistralAI, MistralAI.Types, MistralAI.Functions.Core,
  MistralAI.Httpx;

type
  TToolProc = procedure (const Value: TCalledFunction; Func: IFunctionCore) of object;
  TToolProcEx = procedure (const Value: TMessageOutputEntry; Func: IFunctionCore) of object;

  /// <summary>
  /// Represents a tutorial hub for handling visual components in a Delphi application,
  /// including text display, button interactions, and navigation through pages.
  /// </summary>
  TVCLTutorialHub = class
  private
    FClient: IMistralAI;
    FMemo1: TMemo;
    FMemo2: TMemo;
    FMemo3: TMemo;
    FMemo4: TMemo;
    FButton: TButton;
    FFileId: string;
    FJobId: string;
    FBatchId: string;
    FFileName: string;
    FTool: IFunctionCore;
    FToolCall: TToolProc;
    FToolCallEx: TToolProcEx;
    FPage: Integer;
    FFileOverride: Boolean;
    FCancel: Boolean;
    procedure OnButtonClick(Sender: TObject);
    procedure SetButton(const Value: TButton);
    procedure SetMemo1(const Value: TMemo);
    procedure SetMemo2(const Value: TMemo);
    procedure SetMemo3(const Value: TMemo);
    procedure SetMemo4(const Value: TMemo);
    procedure SetJSONRequest(const Value: string);
    procedure SetJSONResponse(const Value: string);
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
    /// Gets or sets the second memo component for displaying additional messages or data.
    /// </summary>
    property Memo3: TMemo read FMemo3 write SetMemo3;
    /// <summary>
    /// Gets or sets the second memo component for displaying additional messages or data.
    /// </summary>
    property Memo4: TMemo read FMemo4 write SetMemo4;
    /// <summary>
    /// Sets text for displaying JSON request.
    /// </summary>
    property JSONRequest: string write SetJSONRequest;
    /// <summary>
    /// Sets text for displaying JSON response.
    /// </summary>
    property JSONResponse: string write SetJSONResponse;
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

    property ToolCallEx: TToolProcEx read FToolCallEx write FToolCallEx;

    /// <summary>
    /// Gets or sets the current page number within the tutorial.
    /// </summary>
    property Page: Integer read FPage write FPage;
    /// <summary>
    /// Gets or sets a value indicating whether file overrides are allowed.
    /// </summary>
    property FileOverride: Boolean read FFileOverride write FFileOverride;
    procedure JSONRequestClear;
    procedure JSONResponseClear;
    procedure LoadImage(const FilePath: string);
    procedure WeatherFunction(const Value: TCalledFunction; Func: IFunctionCore);
    procedure WeatherFunctionEx(const Value: TMessageOutputEntry; Func: IFunctionCore);
    constructor Create(const AClient: IMistralAI; const AMemo1, AMemo2, AMemo3, AMemo4: TMemo; const AButton: TButton);
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
  procedure DisplayEx(Sender: TObject; Value: TModerationResult); overload;
  procedure Display(Sender: TObject; Value: TModerationResult); overload;
  procedure DisplayEx(Sender: TObject; Value: TModeration); overload;
  procedure Display(Sender: TObject; Value: TModeration); overload;
  procedure Display(Sender: TObject; Value: TFile); overload;
  procedure Display(Sender: TObject; Value: TFiles); overload;
  procedure Display(Sender: TObject; Value: TDeletedResult); overload;
  procedure Display(Sender: TObject; Value: TDownLoadFile); overload;
  procedure Display(Sender: TObject; Value: TJobOut); overload;
  procedure Display(Sender: TObject; Value: TJobOutProgress); overload;
  procedure Display(Sender: TObject; Value: TListFineTuningJobs); overload;
  procedure Display(Sender: TObject; Value: TBatchJob); overload;
  procedure Display(Sender: TObject; Value: TBatchJobList); overload;
  procedure Display(Sender: TObject; Value: TSignedUrl); overload;
  procedure Display(Sender: TObject; Value: TConversation); overload;
  procedure Display(Sender: TObject; Value: TConversationsListItem); overload;
  procedure Display(Sender: TObject; Value: TRetrievedEntries); overload;
  procedure Display(Sender: TObject; Value: TRetrieveMessages); overload;
  procedure Display(Sender: TObject; Value: TOcr); overload;
  procedure Display(Sender: TObject; Value: TConversationsAgent); overload;
  procedure Display(Sender: TObject; Value: TConversationsAgentList); overload;
  procedure Display(Sender: TObject; Value: TConversationsList); overload;
  procedure Display(Sender: TObject; Value: TLibrariesMain); overload;
  procedure Display(Sender: TObject; Value: TLibrariesMainList); overload;
  procedure Display(Sender: TObject; Value: TLibrariesDocuments); overload;
  procedure Display(Sender: TObject; Value: TLibrariesDocumentsList); overload;
  procedure Display(Sender: TObject; Value: TLibraryDocumentsProcessed); overload;
  procedure Display(Sender: TObject; Value: TLibraryDocumentsText); overload;
  procedure Display(Sender: TObject; Value: TLibraryDocumentsStatus); overload;
  procedure Display(Sender: TObject; Value: TLibrariesAccess); overload;
  procedure Display(Sender: TObject; Value: TLibrariesAccessList); overload;

  procedure Display(Sender: TObject; Value: TAudioTranscription); overload;

  procedure DisplayStream(Sender: TObject; Value: string); overload;
  procedure DisplayStream(Sender: TObject; Value: TChat); overload;
  procedure DisplayStream(Sender: TObject; Value: TCodestral); overload;
  procedure DisplayStream(Sender: TObject; Value: TConversationsEvent); overload;

  procedure DisplayChunk(Value: string); overload;
  procedure DisplayChunk(Value: TChat); overload;
  procedure DisplayChunk(Value: TCodestral); overload;
  procedure DisplayChunk(Value: TConversationsEvent); overload;

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
  TutorialHub: TVCLTutorialHub = nil;

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
  Display(Sender, 'The operation was cancelled');
  Display(Sender);
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
    M := TMemo(Sender) else
    M := (Sender as TVCLTutorialHub).Memo1;

  var S := Value.Split([#10]);
  if Length(S) = 0 then
    begin
      M.Lines.Add(Value)
    end
  else
    begin
      for var Item in S do
        M.Lines.Add(Item);
    end;

  M.Perform(WM_VSCROLL, SB_BOTTOM, 0);
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
  TutorialHub.JSONResponse := Value.JSONResponse;
  for var Item in Value.Choices do
    if Item.FinishReason = TFinishReason.tool_calls then
      begin
        if Assigned(TutorialHub.ToolCall) then
          TutorialHub.ToolCall(Item.Message.ToolsCalls[0], TutorialHub.Tool);
      end
    else
      begin
        for var SubItem in Item.Message.Content do
          begin
            case SubItem.&Type of
              TContentType.text:
                Display(Sender, SubItem.Text);
              TContentType.thinking:
                begin
                  for var Think in SubItem.Thinking do
                    Display(TutorialHub.Memo2, Think.Text);
                end;
            end;
          end;
      end;
end;

procedure Display(Sender: TObject; Value: TCodestral); overload;
begin
  TutorialHub.JSONResponse := Value.JSONResponse;
  for var Item in Value.Choices do
    if Item.FinishReason = TCodestralFinishReason.tool_calls then
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
  TutorialHub.JSONResponse := Value.JSONResponse;
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
  if not Value.JSONResponse.IsEmpty then
    TutorialHub.JSONResponse := Value.JSONResponse;
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
  TutorialHub.JSONResponse := Value.JSONResponse;
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
    end
  else
    begin
      Display(Sender, 'No moderation is necessary');
    end;
end;

procedure Display(Sender: TObject; Value: TModeration);
begin
  TutorialHub.JSONResponse := Value.JSONResponse;
  for var Item in Value.Results do
    Display(Sender, Item);
end;

procedure DisplayEx(Sender: TObject; Value: TModeration);
begin
  TutorialHub.JSONResponse := Value.JSONResponse;
  for var Item in Value.Results do
    DisplayEx(Sender, Item);
end;

procedure Display(Sender: TObject; Value: TFile);
begin
  if not Value.JSONResponse.IsEmpty then
    TutorialHub.JSONResponse := Value.JSONResponse;
  Display(Sender, F('id', Value.Id));
  Display(Sender, F('filename', Value.FileName));
  Display(Sender, F('Source', Value.Source.ToString));
  Display(Sender, F('Purpose', Value.Purpose.ToString));
  Display(Sender, F('Object', Value.&Object));
  Display(Sender, F('Bytes', Value.Bytes.ToString));
  Display(Sender, F('mimetype', Value.Mimetype));
  Display(Sender, F('numlines', Value.NumLines.ToString));
  Display(Sender, F('signature', Value.Signature));
  Display(Sender);
end;

procedure Display(Sender: TObject; Value: TFiles);
begin
  TutorialHub.JSONResponse := Value.JSONResponse;
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
  TutorialHub.JSONResponse := Value.JSONResponse;
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
  TutorialHub.JSONResponse := Value.JSONResponse;
  if TutorialHub.FileName.IsEmpty then
    raise Exception.Create('No filename defined for download.');
  Value.SaveToFile(TutorialHub.FileName, not TutorialHub.FileOverride);
  Display(Sender, Format('File downloaded and saved as "%s"', [TutorialHub.FileName]));
end;

procedure Display(Sender: TObject; Value: TJobOut);
begin
  TutorialHub.JSONResponse := Value.JSONResponse;
  Display(Sender, [
    Value.Id,
    F('AutoStart', [
      BoolToStr(Value.AutoStart, True),
      F('Model', Value.Model),
      F('Status', Value.Status.ToString),
      F('FineTuningModel', VartoStr(Value.FineTuningModel))
    ])
  ]);
end;

procedure Display(Sender: TObject; Value: TJobOutProgress);
begin
  TutorialHub.JSONResponse := Value.JSONResponse;
  Display(Sender, [
    Value.Id,
    F('AutoStart', [
      BoolToStr(Value.AutoStart, True),
      F('Model', Value.Model),
      F('Status', Value.Status.ToString),
      F('FineTuningModel', VartoStr(Value.FineTuningModel))
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
  TutorialHub.JSONResponse := Value.JSONResponse;
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
  if not Value.JSONResponse.IsEmpty then
    TutorialHub.JSONResponse := Value.JSONResponse;
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
  TutorialHub.JSONResponse := Value.JSONResponse;
  Display(Sender, F('Total', [
    Value.Total.ToString,
    F('Object', Value.&Object)]));
  for var Item in Value.Data do
    Display(Sender, Item);
end;

procedure Display(Sender: TObject; Value: TSignedUrl);
begin
  TutorialHub.JSONResponse := Value.JSONResponse;
  Display(Sender, Value.Url);
end;

procedure Display(Sender: TObject; Value: TConversation);
begin
  TutorialHub.JSONResponse := Value.JSONResponse;
  for var Item in Value.Outputs do
  begin
    case Item.&Type of
      TConversatonEvent.function_call :
        TutorialHub.ToolCallEx(Item, TutorialHub.Tool);

      TConversatonEvent.message_input,
      TConversatonEvent.message_output:
        begin
          for var SubItem in Item.Content do
            case SubItem.&Type of
              TContentChunkType.text:
                Display(Sender, SubItem.Text);

              TContentChunkType.tool_file:
                begin
                  case Subitem.Tool of
                    TConversationTool.image_generation:
                      TutorialHub.LoadImage(SubItem.FileId);
                  end;
                end;
            end;
        end;
    end;
  end;
end;

procedure Display(Sender: TObject; Value: TConversationsListItem);
begin
  TutorialHub.JSONResponse := Value.JSONResponse;
  if Assigned(Value) then
    begin
      Display(Sender, F('objet', Value.&Object));
      Display(Sender, F('id', Value.Id));
      Display(Sender, F('stop', Value.CompletionArgs.Stop));
    end;
end;

procedure Display(Sender: TObject; Value: TRetrievedEntries); overload;
begin
  TutorialHub.JSONResponse := Value.JSONResponse;
  Display(TutorialHub, F('object', Value.&Object));
  Display(TutorialHub, F('conversation_id', Value.ConversationId));
  Display(TutorialHub, EmptyStr);
  for var Item in Value.Entries do
    begin
      Display(TutorialHub, F('object', Item.&Object));
      Display(TutorialHub, F('type', Item.&Type.ToString));
      Display(TutorialHub, F('id', Item.Id));
      case Item.&Type of
        TConversatonEvent.message_input,
        TConversatonEvent.message_output:
          Display(TutorialHub, F('role', Item.Role.ToString));
        TConversatonEvent.tool_execution:
        Display(TutorialHub, F('function', Item.&Function));
      end;
      Display(TutorialHub, F('prefix', BoolToStr(Item.Prefix, True)));
      for var SubItem in Item.Content do
        begin
          if SubItem.&Type = TContentChunkType.text then
            Display(TutorialHub, F('content', SubItem.Text));
          Display(TutorialHub, EmptyStr);
        end;
      Display(TutorialHub, '----------------');
    end;
end;

procedure Display(Sender: TObject; Value: TRetrieveMessages);
begin
  TutorialHub.JSONResponse := Value.JSONResponse;
  Display(TutorialHub, F('object', Value.&Object));
  Display(TutorialHub, F('conversation_id', Value.ConversationId));
  Display(TutorialHub, EmptyStr);
  for var Item in Value.Messages do
    begin
      Display(TutorialHub, F('object', Item.&Object));
      Display(TutorialHub, F('type', Item.&Type.ToString));
      Display(TutorialHub, F('id', Item.Id));
      case Item.&Type of
        TConversatonEvent.message_input,
        TConversatonEvent.message_output:
          Display(TutorialHub, F('role', Item.Role.ToString));
      end;

      Display(TutorialHub, F('prefix', BoolToStr(Item.Prefix, True)));
      for var SubItem in Item.Content do
        begin
          if SubItem.&Type = TContentChunkType.text then
            Display(TutorialHub, F('content', SubItem.Text));
          Display(TutorialHub, EmptyStr);
        end;
      Display(TutorialHub, '----------------');
    end;
end;

procedure Display(Sender: TObject; Value: TOcr);
begin
  TutorialHub.JSONResponse := Value.JSONResponse;
  for var Item in Value.Pages do
    begin
      Display(Sender, F('page', Item.Index.ToString));
      Display(Sender, Item.Markdown);
    end;
end;

procedure Display(Sender: TObject; Value: TConversationsAgent);
begin
  if not Value.JSONResponse.IsEmpty then
    TutorialHub.JSONResponse := Value.JSONResponse;
  Display(Sender, F('id', Value.Id));
  Display(Sender, F('name', Value.Name));
  if not Value.Model.IsEmpty then
    Display(Sender, F('model', Value.Model));
  if not Value.Description.IsEmpty then
    Display(Sender, F('description', Value.Description));
  if not Value.Instructions.IsEmpty then
    Display(Sender, F('instructions', Value.Instructions));
  Display(Sender, F('version', Value.Version.ToString));
  Display(Sender, EmptyStr);
end;

procedure Display(Sender: TObject; Value: TConversationsAgentList);
begin
  TutorialHub.JSONResponse := Value.JSONResponse;
  for var Item in Value.Data do
    Display(Sender, Item);
end;

procedure Display(Sender: TObject; Value: TConversationsList);
begin
  TutorialHub.JSONResponse := Value.JSONResponse;
  for var Item in Value.Data do
    Display(TutorialHub, Item.Id);
end;

procedure Display(Sender: TObject; Value: TLibrariesMain);
begin
  if not Value.JSONResponse.IsEmpty then
    TutorialHub.JSONResponse := Value.JSONResponse;
  Display(Sender, F('id', F(Value.Id, F('name', Value.Name))));
  Display(Sender, F('chunk_size', Value.ChunkSize.ToString));
  Display(Sender, F('description',Value.Description));
  Display(Sender, EmptyStr);
end;

procedure Display(Sender: TObject; Value: TLibrariesMainList);
begin
  TutorialHub.JSONResponse := Value.JSONResponse;
  for var Item in Value.Data do
    Display(Sender, Item);
end;

procedure Display(Sender: TObject; Value: TLibrariesDocuments);
begin
  if not Value.JSONResponse.IsEmpty then
    TutorialHub.JSONResponse := Value.JSONResponse;
  Display(Sender, F('id', F(Value.Id, F('name', Value.Name))));
  Display(Sender, F('processing_status', F(Value.ProcessingStatus, F('mime_type', Value.MimeType))));
  Display(Sender, F('summary', Value.Summary));
  Display(Sender, EmptyStr);
end;

procedure Display(Sender: TObject; Value: TLibrariesDocumentsList);
begin
  TutorialHub.JSONResponse := Value.JSONResponse;
  Display(Sender,
    F('total_items',
      F(Value.Pagination.TotalItems.ToString,
        F('total_pages', Value.Pagination.TotalPages.ToString))));
  Display(Sender,
    F('current_page',
      F(Value.Pagination.CurrentPage.ToString,
        F('has_more', BoolToStr(Value.Pagination.HasMore, True)))));
  for var Item in Value.Data do
    Display(Sender, Item);
end;

procedure Display(Sender: TObject; Value: TLibraryDocumentsProcessed);
begin
  TutorialHub.JSONResponse := Value.JSONResponse;
  Display(Sender, F('processed', BoolToStr(Value.Processed, True)));
end;

procedure Display(Sender: TObject; Value: TLibraryDocumentsText);
begin
  TutorialHub.JSONResponse := Value.JSONResponse;
  Display(Sender, Value.Text);
end;

procedure Display(Sender: TObject; Value: TLibraryDocumentsStatus);
begin
  TutorialHub.JSONResponse := Value.JSONResponse;
  Display(Sender, F('document_id', Value.DocumentId));
  Display(Sender, F('processing_status', Value.ProcessingStatus));
  Display(Sender, EmptyStr);
end;

procedure Display(Sender: TObject; Value: TLibrariesAccess);
begin
  if not Value.JSONResponse.IsEmpty then
    TutorialHub.JSONResponse := Value.JSONResponse;
  Display(Sender, F('library_id', Value.LibraryId));
  Display(Sender, F('role', Value.Role));
  Display(Sender, F('share_with_uuid', Value.ShareWithUuid));
  Display(Sender, F('share_with_type', Value.ShareWithType.ToString));
  Display(Sender, F('user_id', Value.UserId));
  Display(Sender, EmptyStr);
end;

procedure Display(Sender: TObject; Value: TLibrariesAccessList);
begin
  TutorialHub.JSONResponse := Value.JSONResponse;
  for var Item in Value.Data do
    Display(Sender, Item);
end;

procedure Display(Sender: TObject; Value: TAudioTranscription);
begin
  TutorialHub.JSONResponse := Value.JSONResponse;
  Display(Sender, F('model', Value.Model));
  Display(Sender, F('language', Value.Language));
  Display(Sender, Value.Text);
  Display(Sender);
end;

procedure DisplayStream(Sender: TObject; Value: string);
var
  M: TMemo;
  CurrentLine: string;
  Lines: TArray<string>;
begin
  if Sender is TMemo then
    M := TMemo(Sender) else
    M := (Sender as TVCLTutorialHub).Memo1;
  var OldSelStart := M.SelStart;
  var ShouldScroll := (OldSelStart = M.GetTextLen);
  M.Lines.BeginUpdate;
  try
    Lines := Value.Split([#10]);
    if Length(Lines) > 0 then
    begin
      if M.Lines.Count > 0 then
        CurrentLine := M.Lines[M.Lines.Count - 1]
      else
        CurrentLine := '';
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
  begin
    M.SelStart := M.GetTextLen;
    M.SelLength := 0;
    M.Perform(EM_SCROLLCARET, 0, 0);
  end;
end;

procedure DisplayStream(Sender: TObject; Value: TChat);
begin
  if Assigned(Value) then
    begin
      for var Item in Value.Choices do
        begin
          for var SubItem in Item.Delta.Content do
          begin
            case SubItem.&Type of
              TContentType.text:
                DisplayStream(Sender, SubItem.Text);
              TContentType.thinking:
                begin
                  for var Think in SubItem.Thinking do
                    DisplayStream(TutorialHub.Memo2, Think.Text);
                end;
            end;
          end;

        end;
      DisplayChunk(Value);
    end;
end;

procedure DisplayStream(Sender: TObject; Value: TCodestral); overload;
begin
  if Assigned(Value) then
    begin
      for var Item in Value.Choices do
        begin
          DisplayStream(Sender, Item.Delta.Content);
        end;
      DisplayChunk(Value);
    end;
end;

procedure DisplayStream(Sender: TObject; Value: TConversationsEvent);
begin
  if Assigned(Value) then
    begin
      DisplayChunk(Value);
      if Value.&Type = TChunkEvent.message_output_delta then
        DisplayStream(Sender, Value.Content[0].Text);
    end;
end;

procedure DisplayChunk(Value: string); overload;
begin
  var JSONValue := TJSONObject.ParseJSONValue(Value);
  TutorialHub.Memo4.Lines.BeginUpdate;
  try
    Display(TutorialHub.Memo4, JSONValue.ToString);
  finally
    TutorialHub.Memo4.Lines.EndUpdate;
    JSONValue.Free;
  end;
end;

procedure DisplayChunk(Value: TChat); overload;
begin
  DisplayChunk(Value.JSONResponse);
end;

procedure DisplayChunk(Value: TCodestral); overload;
begin
  DisplayChunk(Value.JSONResponse);
end;

procedure DisplayChunk(Value: TConversationsEvent); overload;
begin
  DisplayChunk(Value.JSONResponse);
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
  Result := Format('%s (%s): %s%%', [Name, BoolToStr(State, True), (Value * 100).ToString(ffNumber, 3, 2)])
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

{ TVCLTutorialHub }

constructor TVCLTutorialHub.Create(const AClient: IMistralAI; const AMemo1, AMemo2, AMemo3, AMemo4: TMemo; const AButton: TButton);
begin
  inherited Create;
  FClient := AClient;
  Memo1 := AMemo1;
  Memo2 := AMemo2;
  Memo3 := AMemo3;
  Memo4 := AMemo4;
  Button := AButton;
  FFileOverride := False;
end;

procedure TVCLTutorialHub.JSONRequestClear;
begin
  Memo3.Clear;
end;

procedure TVCLTutorialHub.JSONResponseClear;
begin
  Memo4.Clear;
end;

procedure TVCLTutorialHub.LoadImage(const FilePath: string);
begin
  FClient.&File.AsyncGetSignedUrl(FilePath,
  procedure (Params: TSignedUrlParams)
  begin
    Params.Expiry(1);
  end,
  function : TAsyncSignedUrl
  begin
    Result.OnSuccess :=
      procedure (Sender: TObject; Value: TSignedUrl)
      begin
        THttpx.DownloadFromSignedUrl(Value.Url, True);
      end;
    Result.OnError := Display;
  end);
end;

procedure TVCLTutorialHub.NextPage;
begin
  Inc(FPage);
end;

procedure TVCLTutorialHub.OnButtonClick(Sender: TObject);
begin
  Cancel := True;
end;

procedure TVCLTutorialHub.SetButton(const Value: TButton);
begin
  FButton := Value;
  FButton.OnClick := OnButtonClick;
  FButton.Caption := 'Cancel';
end;

procedure TVCLTutorialHub.SetJSONRequest(const Value: string);
begin
  Memo3.Lines.Text := Value;
  Memo3.SelStart := 0;
  Application.ProcessMessages;
end;

procedure TVCLTutorialHub.SetJSONResponse(const Value: string);
begin
  Memo4.Lines.Text := Value;
  Memo4.SelStart := 0;
  Application.ProcessMessages;
end;

procedure TVCLTutorialHub.SetMemo1(const Value: TMemo);
begin
  FMemo1 := Value;
  FMemo1.ScrollBars := TScrollStyle.ssVertical;
end;

procedure TVCLTutorialHub.SetMemo2(const Value: TMemo);
begin
  FMemo2 := Value;
  FMemo2.ScrollBars := TScrollStyle.ssVertical;
end;

procedure TVCLTutorialHub.SetMemo3(const Value: TMemo);
begin
  FMemo3 := Value;
  FMemo3.ScrollBars := TScrollStyle.ssBoth;
end;

procedure TVCLTutorialHub.SetMemo4(const Value: TMemo);
begin
  FMemo4 := Value;
  FMemo4.ScrollBars := TScrollStyle.ssBoth;
end;

procedure TVCLTutorialHub.WeatherFunction(const Value: TCalledFunction;
  Func: IFunctionCore);
begin
  var ArgResult := Func.Execute(Value.&Function.Arguments);

  FClient.Chat.AsyncCreateStream(
    procedure (Params: TChatParams)
    begin
      Params.Model('open-mixtral-8x22b-2404');
      Params.Messages([
        PayLoad.System('Respond like a star weather presenter on a prime-time TV channel.'),
        Payload.User(ArgResult)
      ]);
      Params.Stream(True);
      Params.MaxTokens(1024);
    end,
    function : TAsynChatStream
    begin
      Result.Sender := TutorialHub;
      Result.OnProgress := DisplayStream;
      Result.OnSuccess := Display;
      Result.OnDoCancel := DoCancellation;
      Result.OnCancellation := Cancellation;
      Result.OnError := Display;
    end);
end;

procedure TVCLTutorialHub.WeatherFunctionEx(const Value: TMessageOutputEntry;
  Func: IFunctionCore);
begin
  var ArgResult := Func.Execute(Value.Arguments);

  FClient.Conversations.AsyncCreateStream(
    procedure (Params: TConversationsParams)
    begin
      Params.Instructions('Respond like a star weather presenter on a prime-time TV channel.');
      Params.Inputs(ArgResult);
      Params.Model('mistral-medium-2505');
      Params.Stream;
      Params.Store(False);
      Params.CompletionArgs(TCompletionArgsParams.Create
        .Temperature(0.3)
        .TopP(0.95)
      )
    end,
    function : TAsyncConversationsEvent
    begin
      Result.Sender := TutorialHub;
      Result.OnProgress := DisplayStream;
      Result.OnSuccess := Display;
      Result.OnDoCancel := DoCancellation;
      Result.OnCancellation := Cancellation;
      Result.OnError := Display;
    end);
end;

initialization
finalization
  if Assigned(TutorialHub) then
    TutorialHub.Free;
end.
