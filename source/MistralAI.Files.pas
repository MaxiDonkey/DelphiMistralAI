unit MistralAI.Files;

{-------------------------------------------------------------------------------

      Github repository : https://github.com/MaxiDonkey/DelphiMistralAI
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.Classes, System.SysUtils, REST.JsonReflect, REST.Json.Types, System.Net.Mime,
  System.Threading, MistralAI.API.Params, MistralAI.API, MistralAI.Async.Support;

type
  /// <summary>
  /// Specifies the intended purpose of the uploaded file.
  /// </summary>
  /// <remarks>
  /// Currently, only 'finetune' is supported.
  /// </remarks>
  TFilePurpose = (
    /// <summary>
    /// The file will be used for fine-tuning.
    /// </summary>
    finetune
  );

  /// <summary>
  /// Helper methods for <c>TFilePurpose</c> enumeration.
  /// </summary>
  TFilePurposeHelper = record helper for TFilePurpose
    /// <summary>
    /// Converts the <c>TFilePurpose</c> value to its string representation.
    /// </summary>
    /// <returns>
    /// A string representing the <c>TFilePurpose</c> value.
    /// </returns>
    /// <remarks>
    /// For example:
    /// <code>
    /// var
    ///   Purpose: TFilePurpose;
    /// begin
    ///   Purpose := TFilePurpose.finetune;
    ///   ShowMessage(Purpose.ToString); // Outputs 'fine-tune'
    /// end;
    /// </code>
    /// </remarks>
    function ToString: string;
    /// <summary>
    /// Creates a <c>TFilePurpose</c> from its string representation.
    /// </summary>
    /// <param name="Value">
    /// The string representation of the <c>TFilePurpose</c>.
    /// </param>
    /// <returns>
    /// The <c>TFilePurpose</c> corresponding to the specified string.
    /// </returns>
    /// <exception cref="Exception">
    /// Raised if the string does not correspond to any <c>TFilePurpose</c> value.
    /// </exception>
    /// <remarks>
    /// For example:
    /// <code>
    /// var
    ///   Purpose: TFilePurpose;
    /// begin
    ///   Purpose := TFilePurposeHelper.Create('fine-tune');
    ///   // Purpose now equals TFilePurpose.finetune
    /// end;
    /// </code>
    /// </remarks>
    class function Create(const Value: string): TFilePurpose; static;
  end;

  /// <summary>
  /// JSON interceptor to convert <c>TFilePurpose</c> to and from its string representation during JSON serialization.
  /// </summary>
  TFilePurposeInterceptor = class(TJSONInterceptorStringToString)
  public
    /// <summary>
    /// Converts the <c>TFilePurpose</c> field value to its string representation.
    /// </summary>
    /// <param name="Data">
    /// The object containing the field.
    /// </param>
    /// <param name="Field">
    /// The name of the field to convert.
    /// </param>
    /// <returns>
    /// The string representation of the <c>TFilePurpose</c> value.
    /// </returns>
    /// <remarks>
    /// This method is used internally during JSON serialization.
    /// </remarks>
    function StringConverter(Data: TObject; Field: string): string; override;
    /// <summary>
    /// Converts a string to the <c>TFilePurpose</c> field value during JSON deserialization.
    /// </summary>
    /// <param name="Data">
    /// The object containing the field.
    /// </param>
    /// <param name="Field">
    /// The name of the field to convert.
    /// </param>
    /// <param name="Arg">
    /// The string value to convert to <c>TFilePurpose</c>.
    /// </param>
    /// <remarks>
    /// This method is used internally during JSON deserialization.
    /// </remarks>
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  /// <summary>
  /// Parameters for uploading a file.
  /// </summary>
  /// <remarks>
  /// This class is used to construct the multipart form data for file uploads.
  /// Example usage:
  /// <code>
  /// var
  ///   UploadParams: TUploadParams;
  /// begin
  ///   UploadParams := TUploadParams.Create;
  ///   try
  ///     UploadParams.File('path/to/file.jsonl');
  ///     UploadParams.Purpose(TFilePurpose.finetune);
  ///     // Use UploadParams with the API upload method
  ///   finally
  ///     UploadParams.Free;
  ///   end;
  /// end;
  /// </code>
  /// </remarks>
  TUploadParams = class(TMultipartFormData)
    /// <summary>
    /// Adds a file to be uploaded from a file name.
    /// </summary>
    /// <param name="FileName">
    /// The name of the file to upload.
    /// </param>
    /// <returns>
    /// The current <c>TUploadParams</c> instance.
    /// </returns>
    /// <remarks>
    /// Example:
    /// <code>
    /// UploadParams.File('path/to/file.jsonl');
    /// </code>
    /// </remarks>
    function &File(const FileName: string): TUploadParams; overload;
    /// <summary>
    /// Adds a file to be uploaded from a stream.
    /// </summary>
    /// <param name="Stream">
    /// The stream containing the file data.
    /// </param>
    /// <param name="FileName">
    /// The name of the file.
    /// </param>
    /// <returns>
    /// The current <c>TUploadParams</c> instance.
    /// </returns>
    /// <remarks>
    /// Example:
    /// <code>
    /// var
    ///   FileStream: TFileStream;
    /// begin
    ///   FileStream := TFileStream.Create('path/to/file.jsonl', fmOpenRead);
    ///   try
    ///     UploadParams.File(FileStream, 'file.jsonl');
    ///   finally
    ///     FileStream.Free;
    ///   end;
    /// end;
    /// </code>
    /// </remarks>
    function &File(const Stream: TStream; const FileName: string): TUploadParams; overload;
    /// <summary>
    /// Sets the intended purpose of the uploaded file.
    /// </summary>
    /// <param name="Value">
    /// The purpose as a string. Only 'fine-tune' is accepted for now.
    /// </param>
    /// <returns>
    /// The current <c>TUploadParams</c> instance.
    /// </returns>
    /// <remarks>
    /// Example:
    /// <code>
    /// UploadParams.Purpose('fine-tune');
    /// </code>
    /// </remarks>
    function Purpose(const Value: string): TUploadParams; overload;
    /// <summary>
    /// Sets the intended purpose of the uploaded file.
    /// </summary>
    /// <param name="Value">
    /// The purpose as a <c>TFilePurpose</c> value.
    /// </param>
    /// <returns>
    /// The current <c>TUploadParams</c> instance.
    /// </returns>
    /// <remarks>
    /// Example:
    /// <code>
    /// UploadParams.Purpose(TFilePurpose.finetune);
    /// </code>
    /// </remarks>
    function Purpose(const Value: TFilePurpose): TUploadParams; overload;
    /// <summary>
    /// Creates a new instance of <c>TUploadParams</c>.
    /// </summary>
    /// <remarks>
    /// Initializes the multipart form data with ownership set to True.
    /// </remarks>
    constructor Create;  reintroduce;
  end;

  /// <summary>
  /// Represents a file in the Mistral AI system.
  /// </summary>
  /// <remarks>
  /// This class contains properties that represent the attributes of a file as returned by the API.
  /// </remarks>
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
    /// The size (in bytes) of the file.
    /// </summary>
    property Bytes: Int64 read FBytes write FBytes;
    /// <summary>
    /// The UNIX timestamp (in seconds) when the file was created.
    /// </summary>
    /// <remarks>
    /// You can convert this to a TDateTime using standard Delphi functions.
    /// Example:
    /// <code>
    /// var
    ///   CreationDate: TDateTime;
    /// begin
    ///   CreationDate := UnixToDateTime(File.CreatedAt);
    /// end;
    /// </code>
    /// </remarks>
    property CreatedAt: Int64 read FCreated_at write FCreated_at;
    /// <summary>
    /// The name of the uploaded file.
    /// </summary>
    property FileName: string read FFilename write FFilename;
    /// <summary>
    /// The unique identifier of the file.
    /// </summary>
    property Id: string read FId write FId;
    /// <summary>
    /// The object type, which is always 'file'.
    /// </summary>
    property &Object: string read FObject write FObject;
    /// <summary>
    /// The intended purpose of the file.
    /// </summary>
    property Purpose: TFilePurpose read FPurpose write FPurpose;
  end;

  /// <summary>
  /// Represents a collection of files.
  /// </summary>
  /// <remarks>
  /// This class is used to hold a list of files as returned by the API.
  /// Example usage:
  /// <code>
  /// var
  ///   Files: TFiles;
  ///   FileItem: TFile;
  /// begin
  ///   Files := FilesRoute.List;
  ///   try
  ///     for FileItem in Files.Data do
  ///     begin
  ///       // Process each file
  ///     end;
  ///   finally
  ///     Files.Free;
  ///   end;
  /// end;
  /// </code>
  /// </remarks>
  TFiles = class
  private
    [JsonNameAttribute('data')]
    FData: TArray<TFile>;
    [JsonNameAttribute('object')]
    FObject: string;
  public
    /// <summary>
    /// An array of <c>TFile</c> objects representing the files.
    /// </summary>
    property Data: TArray<TFile> read FData write FData;
    /// <summary>
    /// The object type, which is always 'list'.
    /// </summary>
    property &Object: string read FObject write FObject;
    /// <summary>
    /// Destroys the <c>TFiles</c> instance and frees associated resources.
    /// </summary>
    /// <remarks>
    /// This destructor ensures that all <c>TFile</c> instances in the Data array are properly freed.
    /// </remarks>
    destructor Destroy; override;
  end;

  /// <summary>
  /// Represents the result of a file deletion operation.
  /// </summary>
  /// <remarks>
  /// Contains information about the deletion status and the ID of the deleted file.
  /// </remarks>
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
    /// Indicates whether the file was successfully deleted.
    /// </summary>
    property Deleted: Boolean read FDeleted write FDeleted;
    /// <summary>
    /// The unique identifier of the deleted file.
    /// </summary>
    property Id: string read FId write FId;
    /// <summary>
    /// The object type, which is always 'file'.
    /// </summary>
    property &Object: string read FObject write FObject;
  end;

  /// <summary>
  /// Asynchronous callback parameters for operations returning <c>TFiles</c>.
  /// </summary>
  /// <remarks>
  /// Used when performing asynchronous operations that return a <c>TFiles</c> instance.
  /// </remarks>
  TAsynFiles = TAsyncCallBack<TFiles>;

  /// <summary>
  /// Asynchronous callback parameters for operations returning a single <c>TFile</c>.
  /// </summary>
  /// <remarks>
  /// Used when performing asynchronous operations that return a <c>TFile</c> instance.
  /// </remarks>
  TAsynFile = TAsyncCallBack<TFile>;

  /// <summary>
  /// Asynchronous callback parameters for file deletion operations.
  /// </summary>
  /// <remarks>
  /// Used when performing asynchronous operations that return a <c>TDeletedResult</c> instance.
  /// </remarks>
  TAsynFilesDelete = TAsyncCallBack<TDeletedResult>;

  /// <summary>
  /// Provides methods to interact with the Mistral AI Files API.
  /// </summary>
  /// <remarks>
  /// This class includes both synchronous and asynchronous methods for file operations.
  /// Example usage:
  /// <code>
  /// var
  ///   FilesRoute: TFilesRoute;
  ///   FileInfo: TFile;
  /// begin
  ///   FilesRoute := TFilesRoute.Create(APIInstance);
  ///   try
  ///     FileInfo := FilesRoute.Retrieve('file_id');
  ///     // Process FileInfo
  ///   finally
  ///     FilesRoute.Free;
  ///     FileInfo.Free;
  ///   end;
  /// end;
  /// </code>
  /// </remarks>
  TFilesRoute = class(TMistralAIAPIRoute)
    /// <summary>
    /// Asynchronously deletes a file.
    /// </summary>
    /// <param name="FileId">
    /// The unique identifier of the file to delete.
    /// </param>
    /// <param name="CallBacks">
    /// A function that returns the asynchronous callback parameters.
    /// </param>
    /// <remarks>
    /// Example:
    /// <code>
    /// MistralAI.File.AsyncDelete(
    ///   'file_id',
    ///   function: TAsynFilesDelete
    ///   begin
    ///     Result.OnSuccess :=
    ///       procedure(Sender: TObject; DeletedResult: TDeletedResult)
    ///       begin
    ///         // Handle successful deletion
    ///       end;
    ///
    ///     Result.OnError :=
    ///       procedure(Sender: TObject; ErrorMsg: string)
    ///       begin
    ///         // Handle error
    ///       end;
    ///   end);
    /// </code>
    /// </remarks>
    procedure AsyncDelete(const FileId: string;
      const CallBacks: TFunc<TAsynFilesDelete>);
    /// <summary>
    /// Asynchronously retrieves the list of files belonging to the user's organization.
    /// </summary>
    /// <param name="CallBacks">
    /// A function that returns the asynchronous callback parameters.
    /// </param>
    /// <remarks>
    /// Example:
    /// <code>
    /// MistralAI.File.AsyncList(
    ///   function: TAsynFiles
    ///   begin
    ///     Result.OnSuccess :=
    ///       procedure(Sender: TObject; Files: TFiles)
    ///       begin
    ///         // Process files
    ///       end;
    ///
    ///     Result.OnError :=
    ///       procedure(Sender: TObject; ErrorMsg: string)
    ///       begin
    ///         // Handle error
    ///       end;
    ///   end);
    /// </code>
    /// </remarks>
    procedure AsynchList(const CallBacks: TFunc<TAsynFiles>);
    /// <summary>
    /// Asynchronously retrieves information about a specific file.
    /// </summary>
    /// <param name="FileId">
    /// The unique identifier of the file to retrieve.
    /// </param>
    /// <param name="CallBacks">
    /// A function that returns the asynchronous callback parameters.
    /// </param>
    /// <remarks>
    /// Example:
    /// <code>
    /// MistralAI.File.AsyncRetrieve(
    ///   'file_id',
    ///   function: TAsynFile
    ///   begin
    ///     Result.OnSuccess :=
    ///       procedure(Sender: TObject; FileInfo: TFile)
    ///       begin
    ///         // Process FileInfo
    ///       end;
    ///
    ///     Result.OnError :=
    ///       procedure(Sender: TObject; ErrorMsg: string)
    ///       begin
    ///         // Handle error
    ///       end;
    ///   end);
    /// </code>
    /// </remarks>
    procedure AsyncRetrieve(const FileId: string;
      const CallBacks: TFunc<TAsynFile>);
    /// <summary>
    /// Asynchronously uploads a file that can be used across various endpoints.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure used to configure the <c>TUploadParams</c> for the upload.
    /// </param>
    /// <param name="CallBacks">
    /// A function that returns the asynchronous callback parameters.
    /// </param>
    /// <remarks>
    /// The size of individual files can be a maximum of 512 MB. The Fine-tuning API only supports .jsonl files. Please contact support if you need to increase these storage limits.
    /// Example:
    /// <code>
    /// MistralAI.File.AsyncUpload(
    ///   procedure(Params: TUploadParams)
    ///   begin
    ///     Params.File('path/to/file.jsonl');
    ///     Params.Purpose(TFilePurpose.finetune);
    ///   end,
    ///   function: TAsynFile
    ///   begin
    ///     Result.OnSuccess :=
    ///       procedure(Sender: TObject; UploadedFile: TFile)
    ///       begin
    ///         // Process UploadedFile
    ///       end;
    ///
    ///     Result.OnError :=
    ///       procedure(Sender: TObject; ErrorMsg: string)
    ///       begin
    ///         // Handle error
    ///       end;
    ///   end);
    /// </code>
    /// </remarks>
    procedure ASyncUpload(ParamProc: TProc<TUploadParams>;
      const CallBacks: TFunc<TAsynFile>);
    /// <summary>
    /// Deletes a file.
    /// </summary>
    /// <param name="FileId">
    /// The unique identifier of the file to delete.
    /// </param>
    /// <returns>
    /// A <c>TDeletedResult</c> instance representing the result of the deletion.
    /// </returns>
    /// <remarks>
    /// Example:
    /// <code>
    ///   with MistralAI.File.Delete('file_id');
    ///   try
    ///     if Deleted then
    ///       ShowMessage('file deleted');
    ///   finally
    ///     Free;
    ///   end;
    /// </code>
    /// </remarks>
    function Delete(const FileId: string): TDeletedResult;
    /// <summary>
    /// Retrieves the list of files belonging to the user's organization.
    /// </summary>
    /// <returns>
    /// A <c>TFiles</c> instance containing the list of files.
    /// </returns>
    /// <remarks>
    /// Example:
    /// <code>
    ///   var Files := MistralAI.File.List;
    ///   try
    ///     // Process files
    ///   finally
    ///     Files.Free;
    /// end;
    /// </code>
    /// </remarks>
    function List: TFiles;
    /// <summary>
    /// Retrieves information about a specific file.
    /// </summary>
    /// <param name="FileId">
    /// The unique identifier of the file to retrieve.
    /// </param>
    /// <returns>
    /// A <c>TFile</c> instance containing the file information.
    /// </returns>
    /// <remarks>
    /// Example:
    /// <code>
    ///   var FileInfo := MistralAI.File.Retrieve('file_id');
    ///   try
    ///     // Process FileInfo
    ///   finally
    ///     FileInfo.Free;
    ///   end;
    /// </code>
    /// </remarks>
    function Retrieve(const FileId: string): TFile;
    /// <summary>
    /// Uploads a file that can be used across various endpoints.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure used to configure the <c>TUploadParams</c> for the upload.
    /// </param>
    /// <returns>
    /// A <c>TFile</c> instance representing the uploaded file.
    /// </returns>
    /// <remarks>
    /// The size of individual files can be a maximum of 512 MB. The Fine-tuning API only supports .jsonl files. Please contact support if you need to increase these storage limits.
    /// Example:
    /// <code>
    ///   var UploadedFile := MistralAI.File.Upload(
    ///     procedure(Params: TUploadParams)
    ///     begin
    ///       Params.File('path/to/file.jsonl');
    ///       Params.Purpose(TFilePurpose.finetune);
    ///     end);
    ///   try
    ///     // Process UploadedFile
    ///   finally
    ///     UploadedFile.Free;
    ///   end;
    /// </code>
    /// </remarks>
    function Upload(ParamProc: TProc<TUploadParams>): TFile;
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

{ TUploadParams }

function TUploadParams.&File(const FileName: string): TUploadParams;
begin
  AddFile('file', FileName);
  Result := Self;
end;

constructor TUploadParams.Create;
begin
  inherited Create(True);
end;

function TUploadParams.&File(const Stream: TStream;
  const FileName: string): TUploadParams;
begin
  {$IF RTLVersion >= 36.0}
  AddStream('file', Stream, True, FileName);
  {$ELSE}
  AddStream('file', Stream, FileName);
  {$ENDIF}
  Result := Self;
end;

function TUploadParams.Purpose(const Value: string): TUploadParams;
begin
  AddField('purpose', Value);
  Result := Self;
end;

function TUploadParams.Purpose(const Value: TFilePurpose): TUploadParams;
begin
  Result := Purpose(Value.ToString);
end;

{ TFilesRoute }

procedure TFilesRoute.AsyncDelete(const FileId: string;
  const CallBacks: TFunc<TAsynFilesDelete>);
begin
  with TAsyncCallBackExec<TAsynFilesDelete, TDeletedResult>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TDeletedResult
      begin
        Result := Delete(FileId);
      end);
  finally
    Free;
  end;
end;

procedure TFilesRoute.AsynchList(
  const CallBacks: TFunc<TAsynFiles>);
begin
  with TAsyncCallBackExec<TAsynFiles, TFiles>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TFiles
      begin
        Result := List;
      end);
  finally
    Free;
  end;
end;

procedure TFilesRoute.AsyncRetrieve(const FileId: string;
  const CallBacks: TFunc<TAsynFile>);
begin
  with TAsyncCallBackExec<TAsynFile, TFile>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TFile
      begin
        Result := Retrieve(FileId);
      end);
  finally
    Free;
  end;
end;

procedure TFilesRoute.ASyncUpload(ParamProc: TProc<TUploadParams>;
  const CallBacks: TFunc<TAsynFile>);
begin
  with TAsyncCallBackExec<TAsynFile, TFile>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TFile
      begin
        Result := Upload(ParamProc);
      end);
  finally
    Free;
  end;
end;

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

function TFilesRoute.Upload(ParamProc: TProc<TUploadParams>): TFile;
begin
  Result := API.PostForm<TFile, TUploadParams>('files', ParamProc);
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
