unit MistralAI.Files;

interface

uses
  System.Classes, System.SysUtils, REST.JsonReflect, REST.Json.Types, System.Net.Mime,
  MistralAI.API.Params, MistralAI.API;

type
  TFilePurpose = (
    /// <summary>
    /// The file will be used for fine-tuning
    /// </summary>
    finetune
  );

  TFilePurposeHelper = record helper for TFilePurpose
    function ToString: string;
    class function Create(const Value: string): TFilePurpose; static;
  end;

  TFilePurposeInterceptor = class(TJSONInterceptorStringToString)
  public
    function StringConverter(Data: TObject; Field: string): string; override;
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  TFileParams = class(TMultipartFormData)
    /// <summary>
    /// The File object (not file name) to be uploaded
    /// </summary>
    function &File(const FileName: string): TFileParams; overload;
    /// <summary>
    /// The File object (not file name) to be uploaded
    /// </summary>
    function &File(const Stream: TStream; const FileName: string): TFileParams; overload;
    /// <summary>
    /// The intended purpose of the uploaded file. Only accepts fine-tuning (fine-tune) for now
    /// </summary>
    function Purpose(const Value: string): TFileParams; overload;
    /// <summary>
    /// The intended purpose of the uploaded file. Only accepts fine-tuning (fine-tune) for now
    /// </summary>
    function Purpose(const Value: TFilePurpose): TFileParams; overload;
    constructor Create;  reintroduce;
  end;

  TFile = class
  private
    [JsonNameAttribute('bytes')]
    FBytes: Int64;
    [JsonNameAttribute('created_at')]
    FCreated_at: Int64;
    [JsonNameAttribute('filename')]
    FFilename: string;
    [JsonNameAttribute('id')]
    FId: string;
    [JsonNameAttribute('object')]
    FObject: string;
    [JsonReflectAttribute(ctString, rtString, TFilePurposeInterceptor)]
    FPurpose: TFilePurpose;
  public
    /// <summary>
    /// The size (in bytes) of the created file
    /// </summary>
    property Bytes: Int64 read FBytes write FBytes;
    /// <summary>
    /// The UNIX timestamp (in seconds) for the creation time of the file
    /// </summary>
    property CreatedAt: Int64 read FCreated_at write FCreated_at;
    /// <summary>
    /// The name of the file that was uploaded
    /// </summary>
    property FileName: string read FFilename write FFilename;
    /// <summary>
    /// The ID of the created file
    /// </summary>
    property Id: string read FId write FId;
    /// <summary>
    /// The object type, which is always file
    /// </summary>
    property &Object: string read FObject write FObject;
    /// <summary>
    /// any (Purpose)
    /// Value: "fine-tune"
    /// </summary>
    property Purpose: TFilePurpose read FPurpose write FPurpose;
  end;

  TFiles = class
  private
    [JsonNameAttribute('data')]
    FData: TArray<TFile>;
    [JsonNameAttribute('object')]
    FObject: string;
  public
    /// <summary>
    /// Array of objects where each object is of type "TFile". The contents of the table
    /// represent the contents of the remote folder https://api.mistral.ai/v1/files
    /// </summary>
    property Data: TArray<TFile> read FData write FData;
    /// <summary>
    /// The object type, which is always "string"
    /// </summary>
    property &Object: string read FObject write FObject;
    destructor Destroy; override;
  end;

  TDeletedResult = class
  private
    [JsonNameAttribute('deleted')]
    FDeleted: Boolean;
    [JsonNameAttribute('id')]
    FId: string;
    [JsonNameAttribute('object')]
    FObject: string;
  public
    /// <summary>
    /// The deletion status
    /// </summary>
    property Deleted: Boolean read FDeleted write FDeleted;
    /// <summary>
    /// The ID of the deleted file
    /// </summary>
    property Id: string read FId write FId;
    /// <summary>
    /// Default: "file"
    /// The object type that was deleted
    /// </summary>
    property &Object: string read FObject write FObject;
  end;

  TFilesRoute = class(TMistralAIAPIRoute)
    /// <summary>
    /// Delete a file
    /// <param name="FileId">The ID of the file to use for this request</param>
    /// </summary>
    function Delete(const FileId: string): TDeletedResult;
    /// <summary>
    /// Returns a list of files that belong to the user's organization
    /// </summary>
    function List: TFiles;
    /// <summary>
    /// Returns information about a specific file
    /// <param name="FileId">The ID of the file to use for this request</param>
    /// </summary>
    function Retrieve(const FileId: string): TFile;
    /// <summary>
    /// Upload a file that can be used across various endpoints
    /// The size of individual files can be a maximum of 512 MB. The Fine-tuning API only supports .jsonl files
    /// Please contact us if you need to increase these storage limits
    /// </summary>
    function Upload(ParamProc: TProc<TFileParams>): TFile;
  end;

implementation

uses
  System.StrUtils, Rest.Json, System.Rtti;


{ TFilePurposeHelper }

class function TFilePurposeHelper.Create(const Value: string): TFilePurpose;
begin
  case IndexStr(AnsiLowerCase(Value), ['fine-tune']) of
    0 :
      Exit(finetune);
    else
      raise Exception.CreateFmt('(Files: TFilePurpose) %s is not an enum value', [Value]);
  end;
end;

function TFilePurposeHelper.ToString: string;
begin
  case self of
    FineTune:
      Exit('fine-tune');
    else
      raise Exception.Create('(Files) error converting object to string');
  end;
end;

{ TFileParams }

function TFileParams.&File(const FileName: string): TFileParams;
begin
  AddFile('file', FileName);
  Result := Self;
end;

constructor TFileParams.Create;
begin
  inherited Create(True);
end;

function TFileParams.&File(const Stream: TStream;
  const FileName: string): TFileParams;
begin
  AddStream('file', Stream, FileName);
  Result := Self;
end;

function TFileParams.Purpose(const Value: string): TFileParams;
begin
  AddField('purpose', Value);
  Result := Self;
end;

function TFileParams.Purpose(const Value: TFilePurpose): TFileParams;
begin
  Result := Purpose(Value.ToString);
end;

{ TFilesRoute }

function TFilesRoute.Delete(const FileId: string): TDeletedResult;
begin
  Result := API.Delete<TDeletedResult>('files/' + FileId);
end;

function TFilesRoute.List: TFiles;
begin
  Result := API.Get<TFiles>('files');
end;

function TFilesRoute.Retrieve(const FileId: string): TFile;
begin
  Result := API.Get<TFile>('files/' + FileId);
end;

function TFilesRoute.Upload(ParamProc: TProc<TFileParams>): TFile;
begin
  Result := API.PostForm<TFile, TFileParams>('files', ParamProc);
end;

{ TFiles }

destructor TFiles.Destroy;
begin
  for var Item in FData do
    if Assigned(Item) then
      Item.Free;
  inherited;
end;

{ TFilePurposeInterceptor }

function TFilePurposeInterceptor.StringConverter(Data: TObject;
  Field: string): string;
begin
  Result := RTTI.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<TFilePurpose>.ToString;
end;

procedure TFilePurposeInterceptor.StringReverter(Data: TObject; Field,
  Arg: string);
begin
  RTTI.GetType(Data.ClassType).GetField(Field).SetValue(Data, TValue.From(TFilePurpose.Create(Arg)));
end;

end.
