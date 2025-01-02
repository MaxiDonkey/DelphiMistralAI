unit MistralAI.Files;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiMistralAI
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.Classes, System.SysUtils, REST.JsonReflect, REST.Json.Types, System.Net.Mime,
  System.Threading, MistralAI.API.Params, MistralAI.API, MistralAI.Async.Support,
  MistralAI.Types;

type
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
  /// Represents the parameters for listing files with specific criteria.
  /// </summary>
  /// <remarks>
  /// This class is used to configure and pass parameters when listing files from the API.
  /// It provides methods to set pagination, filtering, and searching options.
  /// </remarks>
  TListParams = class(TUrlParam)
  public
    /// <summary>
    /// Sets the intended page in the returned list.
    /// </summary>
    /// <param name="Value">
    /// Integer type size value. Default 0
    /// </param>
    /// <returns>
    /// The current <c>TListParams</c> instance.
    /// </returns>
    function Page(const Value: Integer): TListParams;
    /// <summary>
    /// Sets the intended page size of the returned list.
    /// </summary>
    /// <param name="Value">
    /// Integer type size value. Default 100
    /// </param>
    /// <returns>
    /// The current <c>TListParams</c> instance.
    /// </returns>
    function PageSize(const Value: Integer): TListParams;
    /// <summary>
    /// Sets the intended sample_type of the listed file.
    /// </summary>
    /// <param name="Value">
    /// The sample_type as a <c>TSampleType</c> value.
    /// </param>
    /// <returns>
    /// The current <c>TListParams</c> instance.
    /// </returns>
    function SampleType(const Value: TArray<TSampleType>): TListParams;
    /// <summary>
    /// Sets the intended source of the listed file.
    /// </summary>
    /// <param name="Value">
    /// The source as a <c>TSourceType</c> value.
    /// </param>
    /// <returns>
    /// The current <c>TListParams</c> instance.
    /// </returns>
    function Source(const Value: TArray<TSourceType>): TListParams;
    /// <summary>
    /// Sets the intended search of the listed file.
    /// </summary>
    /// <param name="Value">
    /// The search as a <c>string</c> type value.
    /// </param>
    /// <returns>
    /// The current <c>TListParams</c> instance.
    /// </returns>
    function Search(const Value: string): TListParams;
    /// <summary>
    /// Sets the intended purpose of the listed file.
    /// </summary>
    /// <param name="Value">
    /// The purpose as a <c>TFilePurpose</c> value.
    /// </param>
    /// <returns>
    /// The current <c>TListParams</c> instance.
    /// </returns>
    function Purpose(const Value: TFilePurpose): TListParams;
  end;

  /// <summary>
  /// Represents a file in the Mistral AI system.
  /// </summary>
  /// <remarks>
  /// This class contains properties that represent the attributes of a file as returned by the API.
  /// </remarks>
  TFile = class
  private
    FBytes: Int64;
    [JsonNameAttribute('created_at')]
    FCreated_at: Int64;
    FFilename: string;
    FId: string;
    FObject: string;
    [JsonReflectAttribute(ctString, rtString, TFilePurposeInterceptor)]
    FPurpose: TFilePurpose;
    [JsonReflectAttribute(ctString, rtString, TSampleTypeInterceptor)]
    [JsonNameAttribute('sample_type')]
    FSampleType: TSampleType;
    [JsonNameAttribute('num_lines')]
    FNumLines: Int64;
    [JsonReflectAttribute(ctString, rtString, TSourceTypeInterceptor)]
    FSource: TSourceType;
    FDeleted: Boolean;
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
    /// <summary>
    /// Enum: "pretrain" "instruct" "batch_request" "batch_result" "batch_error"
    /// </summary>
    property SampleType: TSampleType read FSampleType write FSampleType;
    /// <summary>
    /// Num lines returned
    /// </summary>
    property NumLines: Int64 read FNumLines write FNumLines;
    /// <summary>
    /// Enum: "upload" "repository" "mistral"
    /// </summary>
    property Source: TSourceType read FSource write FSource;
    /// <summary>
    /// Indicator when file retrieving only
    /// </summary>
    property Deleted: Boolean read FDeleted write FDeleted;
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
    FData: TArray<TFile>;
    FObject: string;
    FTotal: Int64;
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
    /// The total returned
    /// </summary>
    property Total: Int64 read FTotal write FTotal;
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
  /// Represents a downloaded file, providing functionality to access and save its data.
  /// </summary>
  /// <remarks>
  /// This class encapsulates a downloaded file and provides methods to retrieve its data as a stream
  /// or save it to a file. The file data is expected to be in a base64-encoded format.
  /// </remarks>
  TDownLoadFile = class
  private
    FData: string;
    FFileName: string;
  public
    /// <summary>
    /// Gets the file name where the data was saved.
    /// </summary>
    /// <value>
    /// The file path as a string.
    /// </value>
    /// <remarks>
    /// This property holds the file name specified in the last call to <c>SaveToFile</c>.
    /// </remarks>
    property FileName: string read FFileName write FFileName;
    /// <summary>
    /// Retrieves the downloaded file as a <c>TStream</c>.
    /// </summary>
    /// <returns>
    /// A <c>TStream</c> containing the decoded image data.
    /// </returns>
    /// <remarks>
    /// This method decodes the base64-encoded data and returns it as a stream.
    /// The caller is responsible for freeing the returned stream.
    /// </remarks>
    /// <exception cref="Exception">
    /// Raises an exception data field is empty.
    /// </exception>
    function GetStream: TStream;
    /// <summary>
    /// Saves the gdownloaded file to a file.
    /// </summary>
    /// <param name="FileName">
    /// The file path to save data.
    /// </param>
    /// <remarks>
    /// This method decodes the base64-encoded data and saves it to the specified file.
    /// </remarks>
    /// <exception cref="Exception">
    /// Raises an exception if the data cannot be decoded or saved.
    /// </exception>
    procedure SaveToFile(const FileName: string; const FailIfExists: Boolean = True);
    /// <summary>
    /// File downloaded as base64-encoded data
    /// </summary>
    property Data: string read FData write FData;
  end;

  /// <summary>
  /// Asynchronous callback parameters for operations returning a single <c>TFile</c>.
  /// </summary>
  /// <remarks>
  /// Used when performing asynchronous operations that return a <c>TFile</c> instance.
  /// </remarks>
  TAsynFile = TAsyncCallBack<TFile>;

  /// <summary>
  /// Asynchronous callback parameters for operations returning <c>TFiles</c>.
  /// </summary>
  /// <remarks>
  /// Used when performing asynchronous operations that return a <c>TFiles</c> instance.
  /// </remarks>
  TAsynFiles = TAsyncCallBack<TFiles>;

  /// <summary>
  /// Asynchronous callback parameters for file deletion operations.
  /// </summary>
  /// <remarks>
  /// Used when performing asynchronous operations that return a <c>TDeletedResult</c> instance.
  /// </remarks>
  TAsynFilesDelete = TAsyncCallBack<TDeletedResult>;

  /// <summary>
  /// Asynchronous callback parameters for file download operations.
  /// </summary>
  /// <remarks>
  /// Used when performing asynchronous operations that return a <c>TDownLoadFile</c> instance.
  /// </remarks>
  TAsynDownLoadFile = TAsyncCallBack<TDownLoadFile>;

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
    procedure AsyncDelete(const FileId: string; const CallBacks: TFunc<TAsynFilesDelete>);
    /// <summary>
    /// Asynchronously download a file.
    /// </summary>
    /// <param name="FileId">
    /// The unique identifier of the file to download.
    /// </param>
    /// <param name="CallBacks">
    /// A function that returns the asynchronous callback parameters.
    /// </param>
    /// <remarks>
    /// Example:
    /// <code>
    /// MistralAI.File.AsynDownload(
    ///   'file_id',
    ///   function: TAsynDownLoadFile
    ///   begin
    ///     Result.OnSuccess :=
    ///       procedure(Sender: TObject; Value: TDownLoadFile)
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
    procedure AsyncDownload(const FileId: string; const CallBacks: TFunc<TAsynDownLoadFile>);
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
    procedure AsyncList(CallBacks: TFunc<TAsynFiles>); overload;
    /// <summary>
    /// Asynchronously retrieves the list of files belonging to the user's organization.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure used to configure the <c>TListParams</c> for the upload.
    /// </param>
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
    procedure AsyncList(ParamProc: TProc<TListParams>; CallBacks: TFunc<TAsynFiles>); overload;
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
    procedure AsyncRetrieve(const FileId: string; const CallBacks: TFunc<TAsynFile>);
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
    procedure ASyncUpload(ParamProc: TProc<TUploadParams>; const CallBacks: TFunc<TAsynFile>);
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
    /// Download a file.
    /// </summary>
    /// <param name="FileId">
    /// The unique identifier of the file to delete.
    /// </param>
    /// <returns>
    /// A <c>TDownLoadFile</c> instance representing the result of the download.
    /// </returns>
    /// <remarks>
    /// Example:
    /// <code>
    ///   with MistralAI.File.Download('file_id');
    ///   try
    ///     //do something
    ///   finally
    ///     Free;
    ///   end;
    /// </code>
    /// </remarks>
    function Download(const FileId: string): TDownLoadFile;
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
    function List: TFiles; overload;
    /// <summary>
    /// Retrieves the list of files belonging to the user's organization.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure used to configure the <c>TListParams</c> for the upload.
    /// </param>
    /// <returns>
    /// A <c>TFiles</c> instance containing the list of files.
    /// </returns>
    /// <remarks>
    /// Example:
    /// <code>
    ///   var Files := MistralAI.File.List(
    ///     procedure (Params: TListParams)
    ///     begin
    ///       // Set parameters
    ///     end);
    ///   try
    ///     // Process files
    ///   finally
    ///     Files.Free;
    /// end;
    /// </code>
    /// </remarks>
    function List(ParamProc: TProc<TListParams>): TFiles; overload;
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
  System.StrUtils, Rest.Json, System.Rtti, MistralAI.NetEncoding.Base64, Vcl.Dialogs;

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

procedure TFilesRoute.AsyncList(ParamProc: TProc<TListParams>; CallBacks: TFunc<TAsynFiles>);
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
        Result := List(ParamProc);
      end);
  finally
    Free;
  end;
end;

procedure TFilesRoute.AsyncList(CallBacks: TFunc<TAsynFiles>);
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

procedure TFilesRoute.AsyncDownload(const FileId: string;
  const CallBacks: TFunc<TAsynDownLoadFile>);
begin
  with TAsyncCallBackExec<TAsynDownLoadFile, TDownLoadFile>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TDownLoadFile
      begin
        Result := Self.Download(FileId);
      end);
  finally
    Free;
  end;
end;

function TFilesRoute.Delete(const FileId: string): TDeletedResult;
begin
  Result := API.Delete<TDeletedResult>('files/' + FileId);
end;

function TFilesRoute.Download(const FileId: string): TDownLoadFile;
begin
  Result := API.GetFile<TDownLoadFile>(Format('files/%s/content', [FileId]));
end;

function TFilesRoute.List: TFiles;
begin
  Result := API.Get<TFiles>('files');
end;

function TFilesRoute.List(ParamProc: TProc<TListParams>): TFiles;
begin
  Result := API.Get<TFiles, TListParams>('files', ParamProc);
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

{ TListParams }

function TListParams.Page(const Value: Integer): TListParams;
begin
  Result := TListParams(Add('page', Value));
end;

function TListParams.PageSize(const Value: Integer): TListParams;
begin
  Result := TListParams(Add('page_size', Value));
end;

function TListParams.Purpose(const Value: TFilePurpose): TListParams;
begin
  Result := TListParams(Add('purpose', Value.ToString));
end;

function TListParams.SampleType(const Value: TArray<TSampleType>): TListParams;
begin
  var Param: TArray<string> := [];
  for var Item in Value do
    Param := Param + [Item.ToString];
  Result := TListParams(Add('sample_type', Param));
end;

function TListParams.Search(const Value: string): TListParams;
begin
  Result := TListParams(Add('search', Value));
end;

function TListParams.Source(const Value: TArray<TSourceType>): TListParams;
begin
  var Param: TArray<string> := [];
  for var Item in Value do
    Param := Param + [Item.ToString];
  Result := TListParams(Add('source', Param));
end;

{ TDownLoadFile }

function TDownLoadFile.GetStream: TStream;
begin
  {--- Create a memory stream to write the decoded content. }
  Result := TMemoryStream.Create;
  try
    {--- Convert the base-64 string directly into the memory stream. }
    DecodeBase64ToStream(Data, Result)
  except
    Result.Free;
    raise;
  end;
end;

procedure TDownLoadFile.SaveToFile(const FileName: string; const FailIfExists: Boolean);
begin
  if FileExists(FileName) and FailIfExists then
    raise Exception.CreateFmt('The file "%s" already exists and has not been overwritten.', [FileName]);
  try
    Self.FFileName := FileName;
    {--- Perform the decoding operation and save it into the file specified by the FileName parameter. }
    DecodeBase64ToFile(Data, FileName)
  except
    raise;
  end;
end;

end.
