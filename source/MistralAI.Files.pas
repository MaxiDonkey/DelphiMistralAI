unit MistralAI.Files;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiMistralAI
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.Classes, System.SysUtils, REST.JsonReflect, REST.Json.Types, System.Net.Mime,
  System.Threading, MistralAI.API.Params, MistralAI.API, MistralAI.Async.Support,
  MistralAI.Types, MistralAI.Async.Promise;

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
    constructor Create; reintroduce;
  end;

  /// <summary>
  /// Represents the parameters for listing files with specific criteria.
  /// </summary>
  /// <remarks>
  /// This class is used to configure and pass parameters when listing files from the API.
  /// It provides methods to set pagination, filtering, and searching options.
  /// </remarks>
  TListParams = class(TUrlParam)
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
  /// Represents URL signing parameters for file access requests.
  /// </summary>
  /// <remarks>
  /// Use this class to specify how long a pre-signed URL remains valid.
  /// The default expiry is 24 hours.
  /// </remarks>
  TSignedUrlParams = class(TUrlParam)
    /// <summary>
    /// Sets the number of hours before the signed URL expires.
    /// </summary>
    /// <param name="Value">
    /// The lifetime of the URL in hours. If not specified, defaults to 24.
    /// </param>
    /// <returns>
    /// The current <c>TSignedUrlParams</c> instance.
    /// </returns>
    /// <remarks>
    /// After the expiry time elapses, the signed URL will no longer grant access.
    /// </remarks>
    function Expiry(const Value: Integer): TSignedUrlParams;
  end;

  /// <summary>
  /// Represents a file in the Mistral AI system.
  /// </summary>
  /// <remarks>
  /// This class contains properties that represent the attributes of a file as returned by the API.
  /// </remarks>
  TFile = class(TJSONFingerprint)
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
    FMimetype: string;
    [JsonReflectAttribute(ctString, rtString, TSourceTypeInterceptor)]
    FSource: TSourceType;
    FDeleted: Boolean;
    FSignature: string;
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

    property Mimetype: string read FMimetype write FMimetype;

    /// <summary>
    /// Enum: "upload" "repository" "mistral"
    /// </summary>
    property Source: TSourceType read FSource write FSource;

    /// <summary>
    /// Indicator when file retrieving only
    /// </summary>
    property Deleted: Boolean read FDeleted write FDeleted;

    property Signature: string read FSignature write FSignature;
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
  TFiles = class(TJSONFingerprint)
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
  TDeletedResult = class(TJSONFingerprint)
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
  TDownLoadFile = class(TJSONFingerprint)
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
  /// Represents a pre-signed URL returned by the API for temporary file access.
  /// </summary>
  /// <remarks>
  /// This class holds the URL that grants time-limited access to a file resource.
  /// Use the <see cref="Url"/> property before it expires.
  /// </remarks>
  TSignedUrl = class(TJSONFingerprint)
  private
    FUrl: string;
  public
    /// <summary>
    /// Gets or sets the pre-signed URL.
    /// </summary>
    /// <value>
    /// A string containing the URL that allows temporary access to the file.
    /// </value>
    /// <remarks>
    /// The URL is valid for the duration specified when it was created.
    /// </remarks>
    property Url: string read FUrl write FUrl;
  end;

  /// <summary>
  /// Asynchronous callback parameters for operations returning a single <c>TFile</c>.
  /// </summary>
  /// <remarks>
  /// Used when performing asynchronous operations that return a <c>TFile</c> instance.
  /// </remarks>
  TAsyncFile = TAsyncCallback<TFile>;

  /// <summary>
  /// Promise-based callback parameters for operations returning a single <c>TFile</c>.
  /// </summary>
  /// <remarks>
  /// Use this type when you prefer a promise-style API over traditional Callbacks.
  /// </remarks>
  TPromiseFile = TPromiseCallback<TFile>;

  /// <summary>
  /// Asynchronous callback parameters for operations returning <c>TFiles</c>.
  /// </summary>
  /// <remarks>
  /// Used when performing asynchronous operations that return a <c>TFiles</c> instance.
  /// </remarks>
  TAsyncFiles = TAsyncCallback<TFiles>;

  /// <summary>
  /// Promise-based callback parameters for operations returning a collection of <c>TFile</c> objects.
  /// </summary>
  /// <remarks>
  /// Use this type when you prefer a promise-style API over traditional Callbacks for file list operations.
  /// </remarks>
  TPromiseFiles = TPromiseCallback<TFiles>;

  /// <summary>
  /// Asynchronous callback parameters for file deletion operations.
  /// </summary>
  /// <remarks>
  /// Used when performing asynchronous operations that return a <c>TDeletedResult</c> instance.
  /// </remarks>
  TAsyncFilesDelete = TAsyncCallback<TDeletedResult>;

  /// <summary>
  /// Promise-based callback parameters for operations that delete a file, returning a <c>TDeletedResult</c>.
  /// </summary>
  /// <remarks>
  /// Use this type when you prefer a promise-style API over traditional Callbacks for file deletion operations.
  /// </remarks>
  TPromiseFilesDelete = TPromiseCallback<TDeletedResult>;

  /// <summary>
  /// Asynchronous callback parameters for file download operations.
  /// </summary>
  /// <remarks>
  /// Used when performing asynchronous operations that return a <c>TDownLoadFile</c> instance.
  /// </remarks>
  TAsyncDownLoadFile = TAsyncCallback<TDownLoadFile>;

  /// <summary>
  /// Promise-based callback parameters for operations returning a downloaded <c>TDownLoadFile</c>.
  /// </summary>
  /// <remarks>
  /// Use this type when you prefer a promise-style API over traditional Callbacks for file download operations.
  /// </remarks>
  TPromiseDownLoadFile = TPromiseCallback<TDownLoadFile>;

  /// <summary>
  /// Asynchronous callback parameters for operations returning a <c>TSignedUrl</c>.
  /// </summary>
  /// <remarks>
  /// Use this type when performing an asynchronous request to obtain a pre-signed URL.
  /// Configure its <c>OnSuccess</c> and <c>OnError</c> handlers to handle the result or any errors.
  /// Example:
  /// <code>
  /// MistralAI.File.AsyncGetSignedUrl(
  ///   'file_id',
  ///   procedure(Params: TSignedUrlParams)
  ///   begin
  ///     Params.Expiry(24);
  ///   end,
  ///   function: TAsyncSignedUrl
  ///   begin
  ///     Result.OnSuccess :=
  ///       procedure(Sender: TObject; UrlResult: TSignedUrl)
  ///       begin
  ///         // Use UrlResult.Url
  ///       end;
  ///     Result.OnError :=
  ///       procedure(Sender: TObject; ErrorMsg: string)
  ///       begin
  ///         // Handle error
  ///       end;
  ///   end);
  /// </code>
  /// </remarks>
  TAsyncSignedUrl = TAsyncCallback<TSignedUrl>;

  /// <summary>
  /// Promise-style callback parameters for operations returning a <c>TSignedUrl</c>.
  /// </summary>
  /// <remarks>
  /// Use this type when you prefer a promise-based API to request a pre-signed URL.
  /// The returned promise exposes <c>Then</c> for success and <c>Catch</c> for error handling.
  /// Example:
  /// <code>
  /// FilesRoute.AsyncAwaitGetSignedUrl(
  ///   'file_id',
  ///   procedure(Params: TSignedUrlParams)
  ///   begin
  ///     Params.Expiry(24);
  ///   end
  /// ).&Then(
  ///   function(UrlResult: TSignedUrl): TSignedUrl
  ///   begin
  ///     // Use UrlResult.Url
  ///   end
  /// ).&Catch(
  ///   procedure(E: Exception)
  ///   begin
  ///     // Handle error
  ///   end
  /// );
  /// </code>
  /// </remarks>
  TPromiseSignedUrl = TPromiseCallback<TSignedUrl>;

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
    /// Wraps the asynchronous delete operation in a promise-style API.
    /// </summary>
    /// <param name="FileId">
    /// The unique identifier of the file to delete.
    /// </param>
    /// <param name="Callbacks">
    /// Optional function to configure promise-based Callbacks (OnSuccess, OnError, etc.).
    /// </param>
    /// <returns>
    /// A <see cref="TPromise&lt;TDeletedResult&gt;"/> that resolves with the deletion result.
    /// </returns>
    /// <remarks>
    /// Internally calls <see cref="AsyncDelete"/> and wraps its Callbacks into a <see cref="TPromise&lt;TDeletedResult&gt;"/>.
    /// Use this method when you prefer awaiting a promise rather than handling Callbacks directly.
    /// </remarks>
    function AsyncAwaitDelete(const FileId: string;
      const Callbacks: TFunc<TPromiseFilesDelete> = nil): TPromise<TDeletedResult>;

    /// <summary>
    /// Wraps the asynchronous download operation in a promise-style API.
    /// </summary>
    /// <param name="FileId">
    /// The unique identifier of the file to download.
    /// </param>
    /// <param name="Callbacks">
    /// Optional function to configure promise-based Callbacks (OnSuccess, OnError, etc.).
    /// </param>
    /// <returns>
    /// A <see cref="TPromise&lt;TDownLoadFile&gt;"/> that resolves with the downloaded file.
    /// </returns>
    /// <remarks>
    /// Internally calls <see cref="AsyncDownload"/> and wraps its Callbacks into a <see cref="TPromise&lt;TDownLoadFile&gt;"/>.
    /// Use this method when you prefer awaiting a promise rather than handling Callbacks directly.
    /// </remarks>
    function AsyncAwaitDownload(const FileId: string;
      const Callbacks: TFunc<TPromiseDownLoadFile> = nil): TPromise<TDownLoadFile>;

    /// <summary>
    /// Wraps the asynchronous list operation in a promise-style API.
    /// </summary>
    /// <param name="Callbacks">
    /// Optional function to configure promise-based Callbacks (OnSuccess, OnError, etc.).
    /// </param>
    /// <returns>
    /// A <see cref="TPromise&lt;TFiles&gt;"/> that resolves with the collection of files.
    /// </returns>
    /// <remarks>
    /// Internally calls <see cref="AsyncList"/> and wraps its Callbacks into a <see cref="TPromise&lt;TFiles&gt;"/>.
    /// Use this method when you prefer awaiting a promise rather than handling Callbacks directly.
    /// </remarks>
    function AsyncAwaitList(
      const Callbacks: TFunc<TPromiseFiles> = nil): TPromise<TFiles>; overload;

    /// <summary>
    /// Wraps the asynchronous list operation—with custom parameters—in a promise-style API.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure to configure the <see cref="TListParams"/> (e.g., paging, filtering).
    /// </param>
    /// <param name="Callbacks">
    /// Optional function to configure promise-based Callbacks (OnSuccess, OnError, etc.).
    /// </param>
    /// <returns>
    /// A <see cref="TPromise&lt;TFiles&gt;"/> that resolves with the collection of files matching the specified parameters.
    /// </returns>
    /// <remarks>
    /// Internally calls <see cref="AsyncList"/> with <paramref name="ParamProc"/> and wraps its Callbacks into a <see cref="TPromise&lt;TFiles&gt;"/>.
    /// Use this overload when you need to customize list parameters before awaiting the result.
    /// </remarks>
    function AsyncAwaitList(const ParamProc: TProc<TListParams>;
      const Callbacks: TFunc<TPromiseFiles> = nil): TPromise<TFiles>; overload;

    /// <summary>
    /// Wraps the asynchronous retrieve operation in a promise-style API.
    /// </summary>
    /// <param name="FileId">
    /// The unique identifier of the file to retrieve.
    /// </param>
    /// <param name="Callbacks">
    /// Optional function to configure promise-based Callbacks (OnSuccess, OnError, etc.).
    /// </param>
    /// <returns>
    /// A <see cref="TPromise&lt;TFile&gt;"/> that resolves with the retrieved file.
    /// </returns>
    /// <remarks>
    /// Internally calls <see cref="AsyncRetrieve"/> and wraps its Callbacks into a <see cref="TPromise&lt;TFile&gt;"/>.
    /// Use this method when you prefer awaiting a promise rather than handling Callbacks directly.
    /// </remarks>
    function AsyncAwaitRetrieve(const FileId: string;
      const Callbacks: TFunc<TPromiseFile> = nil): TPromise<TFile>;

    /// <summary>
    /// Wraps the asynchronous upload operation in a promise-style API.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure to configure the <see cref="TUploadParams"/> (e.g., file stream, purpose).
    /// </param>
    /// <param name="Callbacks">
    /// Optional function to configure promise-based Callbacks (OnSuccess, OnError, etc.).
    /// </param>
    /// <returns>
    /// A <see cref="TPromise&lt;TFile&gt;"/> that resolves with the uploaded file metadata.
    /// </returns>
    /// <remarks>
    /// Internally calls <see cref="AsyncUpload"/> with <paramref name="ParamProc"/> and wraps its Callbacks into a <see cref="TPromise&lt;TFile&gt;"/>.
    /// Use this method when you prefer awaiting a promise rather than handling Callbacks directly.
    /// </remarks>
    function AsyncAwaitUpload(const ParamProc: TProc<TUploadParams>;
      const Callbacks: TFunc<TPromiseFile> = nil): TPromise<TFile>;

    /// <summary>
    /// Wraps the asynchronous get-signed-URL operation in a promise-style API.
    /// </summary>
    /// <param name="FileId">
    /// The unique identifier of the file for which to obtain a signed URL.
    /// </param>
    /// <param name="ParamProc">
    /// A procedure to configure the <c>TSignedUrlParams</c> (e.g., expiry time).
    /// </param>
    /// <param name="Callbacks">
    /// Optional function to configure promise-based Callbacks (<c>OnSuccess</c>, <c>OnError</c>, etc.).
    /// </param>
    /// <returns>
    /// A <c>TPromise&lt;TSignedUrl&gt;</c> that resolves with the pre-signed URL.
    /// </returns>
    /// <remarks>
    /// Internally calls <see cref="ASyncGetSignedUrl"/> and wraps its Callbacks into a promise.
    /// Use <c>Then</c> to handle success and <c>Catch</c> to handle errors.
    /// Example:
    /// <code>
    /// FilesRoute.AsyncAwaitGetSignedUrl(
    ///   'file_id',
    ///   procedure(Params: TSignedUrlParams)
    ///   begin
    ///     Params.Expiry(24);
    ///   end
    /// ).Then(
    ///   function(UrlResult: TSignedUrl): TSignedUrl
    ///   begin
    ///     // Use UrlResult.Url here
    ///   end
    /// ).Catch(
    ///   procedure(E: Exception)
    ///   begin
    ///     // Handle error
    ///   end
    /// );
    /// </code>
    /// </remarks>
    function AsyncAwaitGetSignedUrl(const FileId: string;
      const ParamProc: TProc<TSignedUrlParams>;
      const Callbacks: TFunc<TPromiseSignedUrl> = nil): TPromise<TSignedUrl>;

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
    function List(const ParamProc: TProc<TListParams>): TFiles; overload;

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
    function Upload(const ParamProc: TProc<TUploadParams>): TFile;

    /// <summary>
    /// Retrieves a pre-signed URL for temporary access to the specified file.
    /// </summary>
    /// <param name="FileId">
    /// The unique identifier of the file to generate a signed URL for.
    /// </param>
    /// <param name="ParamProc">
    /// A procedure to configure the <c>TSignedUrlParams</c> (e.g., expiry time in hours).
    /// </param>
    /// <returns>
    /// A <c>TSignedUrl</c> instance containing the URL that grants time-limited access.
    /// </returns>
    /// <remarks>
    /// Internally invokes the API endpoint <c>files/{FileId}/url</c> with the given parameters.
    /// Example:
    /// <code>
    /// var
    ///   UrlResult: TSignedUrl;
    /// begin
    ///   UrlResult := FilesRoute.GetSignedUrl('file_id',
    ///     procedure(Params: TSignedUrlParams)
    ///     begin
    ///       Params.Expiry(24);
    ///     end);
    ///   try
    ///     // Use UrlResult.Url here
    ///   finally
    ///     UrlResult.Free;
    ///   end;
    /// end;
    /// </remarks>
    function GetSignedUrl(const FileId: string;
      const ParamProc: TProc<TSignedUrlParams>): TSignedUrl;

    /// <summary>
    /// Asynchronously deletes a file.
    /// </summary>
    /// <param name="FileId">
    /// The unique identifier of the file to delete.
    /// </param>
    /// <param name="Callbacks">
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
    procedure AsyncDelete(const FileId: string; const Callbacks: TFunc<TAsyncFilesDelete>);

    /// <summary>
    /// Asynchronously download a file.
    /// </summary>
    /// <param name="FileId">
    /// The unique identifier of the file to download.
    /// </param>
    /// <param name="Callbacks">
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
    procedure AsyncDownload(const FileId: string; const Callbacks: TFunc<TAsyncDownLoadFile>);

    /// <summary>
    /// Asynchronously retrieves the list of files belonging to the user's organization.
    /// </summary>
    /// <param name="Callbacks">
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
    procedure AsyncList(const Callbacks: TFunc<TAsyncFiles>); overload;

    /// <summary>
    /// Asynchronously retrieves the list of files belonging to the user's organization.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure used to configure the <c>TListParams</c> for the upload.
    /// </param>
    /// <param name="Callbacks">
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
    procedure AsyncList(const ParamProc: TProc<TListParams>; const Callbacks: TFunc<TAsyncFiles>); overload;

    /// <summary>
    /// Asynchronously retrieves information about a specific file.
    /// </summary>
    /// <param name="FileId">
    /// The unique identifier of the file to retrieve.
    /// </param>
    /// <param name="Callbacks">
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
    procedure AsyncRetrieve(const FileId: string; const Callbacks: TFunc<TAsyncFile>);

    /// <summary>
    /// Asynchronously uploads a file that can be used across various endpoints.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure used to configure the <c>TUploadParams</c> for the upload.
    /// </param>
    /// <param name="Callbacks">
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
    procedure ASyncUpload(const ParamProc: TProc<TUploadParams>; const Callbacks: TFunc<TAsyncFile>);

    /// <summary>
    /// Asynchronously obtains a pre-signed URL for temporary file access.
    /// </summary>
    /// <param name="FileId">
    /// The unique identifier of the file for which to generate the signed URL.
    /// </param>
    /// <param name="ParamProc">
    /// A procedure to configure the <c>TSignedUrlParams</c>, such as the URL expiry time.
    /// </param>
    /// <param name="Callbacks">
    /// A function that returns the asynchronous callback parameters (<c>OnStart</c>, <c>OnSuccess</c>, <c>OnError</c>).
    /// </param>
    /// <remarks>
    /// This method invokes <see cref="GetSignedUrl"/> under the hood and delivers the result via the provided Callbacks.
    /// Use <c>Result.OnSuccess</c> to receive the <c>TSignedUrl</c> instance, or <c>Result.OnError</c> to handle failures.
    /// Example:
    /// <code>
    /// FilesRoute.ASyncGetSignedUrl(
    ///   'file_id',
    ///   procedure(Params: TSignedUrlParams)
    ///   begin
    ///     Params.Expiry(24);
    ///   end,
    ///   function: TAsyncSignedUrl
    ///   begin
    ///     Result.OnSuccess :=
    ///       procedure(Sender: TObject; UrlResult: TSignedUrl)
    ///       begin
    ///         // Use UrlResult.Url
    ///       end;
    ///     Result.OnError :=
    ///       procedure(Sender: TObject; ErrorMsg: string)
    ///       begin
    ///         // Handle error
    ///       end;
    ///   end
    /// );
    /// </code>
    /// </remarks>
    procedure ASyncGetSignedUrl(const FileId: string;
      const ParamProc: TProc<TSignedUrlParams>;
      const Callbacks: TFunc<TAsyncSignedUrl>);
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
  {$IF RTLVersion > 35.0}
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

function TFilesRoute.AsyncAwaitDelete(const FileId: string;
  const Callbacks: TFunc<TPromiseFilesDelete>): TPromise<TDeletedResult>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TDeletedResult>(
    procedure(const CallbackParams: TFunc<TAsyncFilesDelete>)
    begin
      AsyncDelete(FileId, CallbackParams);
    end,
    Callbacks);
end;

function TFilesRoute.AsyncAwaitDownload(const FileId: string;
  const Callbacks: TFunc<TPromiseDownLoadFile>): TPromise<TDownLoadFile>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TDownLoadFile>(
    procedure(const CallbackParams: TFunc<TAsyncDownLoadFile>)
    begin
      AsyncDownload(FileId, CallbackParams);
    end,
    Callbacks);
end;

function TFilesRoute.AsyncAwaitGetSignedUrl(const FileId: string;
  const ParamProc: TProc<TSignedUrlParams>;
  const Callbacks: TFunc<TPromiseSignedUrl>): TPromise<TSignedUrl>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TSignedUrl>(
    procedure(const CallbackParams: TFunc<TAsyncSignedUrl>)
    begin
      ASyncGetSignedUrl(FileId, ParamProc, CallbackParams);
    end,
    Callbacks);
end;

function TFilesRoute.AsyncAwaitList(const ParamProc: TProc<TListParams>;
  const Callbacks: TFunc<TPromiseFiles>): TPromise<TFiles>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TFiles>(
    procedure(const CallbackParams: TFunc<TAsyncFiles>)
    begin
      AsyncList(ParamProc, CallbackParams);
    end,
    Callbacks);
end;

function TFilesRoute.AsyncAwaitList(
  const Callbacks: TFunc<TPromiseFiles>): TPromise<TFiles>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TFiles>(
    procedure(const CallbackParams: TFunc<TAsyncFiles>)
    begin
      AsyncList(CallbackParams);
    end,
    Callbacks);
end;

function TFilesRoute.AsyncAwaitRetrieve(const FileId: string;
  const Callbacks: TFunc<TPromiseFile>): TPromise<TFile>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TFile>(
    procedure(const CallbackParams: TFunc<TAsyncFile>)
    begin
      AsyncRetrieve(FileId, CallbackParams);
    end,
    Callbacks);
end;

function TFilesRoute.AsyncAwaitUpload(const ParamProc: TProc<TUploadParams>;
  const Callbacks: TFunc<TPromiseFile>): TPromise<TFile>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TFile>(
    procedure(const CallbackParams: TFunc<TAsyncFile>)
    begin
      AsyncUpload(ParamProc, CallbackParams);
    end,
    Callbacks);
end;

procedure TFilesRoute.AsyncDelete(const FileId: string;
  const Callbacks: TFunc<TAsyncFilesDelete>);
begin
  with TAsyncCallBackExec<TAsyncFilesDelete, TDeletedResult>.Create(Callbacks) do
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

procedure TFilesRoute.AsyncList(const ParamProc: TProc<TListParams>;
  const Callbacks: TFunc<TAsyncFiles>);
begin
  with TAsyncCallBackExec<TAsyncFiles, TFiles>.Create(Callbacks) do
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

procedure TFilesRoute.AsyncList(const Callbacks: TFunc<TAsyncFiles>);
begin
  with TAsyncCallBackExec<TAsyncFiles, TFiles>.Create(Callbacks) do
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
  const Callbacks: TFunc<TAsyncFile>);
begin
  with TAsyncCallBackExec<TAsyncFile, TFile>.Create(Callbacks) do
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

procedure TFilesRoute.ASyncUpload(const ParamProc: TProc<TUploadParams>;
  const Callbacks: TFunc<TAsyncFile>);
begin
  with TAsyncCallBackExec<TAsyncFile, TFile>.Create(Callbacks) do
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
  const Callbacks: TFunc<TAsyncDownLoadFile>);
begin
  with TAsyncCallBackExec<TAsyncDownLoadFile, TDownLoadFile>.Create(Callbacks) do
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

procedure TFilesRoute.ASyncGetSignedUrl(const FileId: string;
  const ParamProc: TProc<TSignedUrlParams>;
  const Callbacks: TFunc<TAsyncSignedUrl>);
begin
  with TAsyncCallBackExec<TAsyncSignedUrl, TSignedUrl>.Create(Callbacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TSignedUrl
      begin
        Result := GetSignedUrl(FileId, ParamProc);
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

function TFilesRoute.GetSignedUrl(const FileId: string;
  const ParamProc: TProc<TSignedUrlParams>): TSignedUrl;
begin
  Result := API.Get<TSignedUrl, TSignedUrlParams>('files/' + FileId + '/url', ParamProc);
end;

function TFilesRoute.List: TFiles;
begin
  Result := API.Get<TFiles>('files');
end;

function TFilesRoute.List(const ParamProc: TProc<TListParams>): TFiles;
begin
  Result := API.Get<TFiles, TListParams>('files', ParamProc);
end;

function TFilesRoute.Retrieve(const FileId: string): TFile;
begin
  Result := API.Get<TFile>('files/' + FileId);
end;

function TFilesRoute.Upload(const ParamProc: TProc<TUploadParams>): TFile;
begin
  Result := API.PostForm<TFile, TUploadParams>('files', ParamProc);
end;

{ TFiles }

destructor TFiles.Destroy;
begin
  for var Item in FData do
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

{ TSignedUrlParams }

function TSignedUrlParams.Expiry(const Value: Integer): TSignedUrlParams;
begin
  Result := TSignedUrlParams(Add('expiry', Value));
end;

end.
