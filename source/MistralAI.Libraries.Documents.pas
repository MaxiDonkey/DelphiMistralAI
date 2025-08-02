unit MistralAI.Libraries.Documents;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiMistralAI
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, System.Classes, System.Threading, System.JSON, System.Net.Mime,
  REST.Json.Types, REST.JsonReflect,
  MistralAI.API.Params, MistralAI.API, MistralAI.Types, MistralAI.Files,
  MistralAI.Async.Params, MistralAI.Async.Support, MistralAI.Async.Promise;

type
  /// <summary>
  /// Represents the URL query parameters used when listing documents in a Mistral library.
  /// </summary>
  /// <remarks>
  /// This class provides a fluent interface to configure pagination, sorting, and search options
  /// for document retrieval via the <c>/v1/libraries/{library_id}/documents</c> endpoint.
  /// </remarks>
  TLibrariesDocumentsUrlParams = class(TUrlParam)
    /// <summary>
    /// Sets the search filter to match document names or content.
    /// </summary>
    /// <param name="Value">
    /// A keyword or phrase to search for, or an empty string for no filtering.
    /// </param>
    /// <returns>
    /// The current <c>TLibrariesDocumentsUrlParams</c> instance for method chaining.
    /// </returns>
    function Search(const Value: string): TLibrariesDocumentsUrlParams;

    /// <summary>
    /// Sets the number of documents to return per page.
    /// </summary>
    /// <param name="Value">
    /// The maximum number of documents to return per page. Defaults to 100.
    /// </param>
    /// <returns>
    /// The current <c>TLibrariesDocumentsUrlParams</c> instance for method chaining.
    /// </returns>
    function PageSize(const Value: Integer = 100): TLibrariesDocumentsUrlParams;

    /// <summary>
    /// Sets the page number to retrieve.
    /// </summary>
    /// <param name="Value">
    /// The page index to retrieve, starting from 0. Defaults to 0.
    /// </param>
    /// <returns>
    /// The current <c>TLibrariesDocumentsUrlParams</c> instance for method chaining.
    /// </returns>
    function Page(const Value: Integer = 0): TLibrariesDocumentsUrlParams;

    /// <summary>
    /// Sets the field by which to sort the document list.
    /// </summary>
    /// <param name="Value">
    /// The field to sort by. Defaults to <c>"created_at"</c>.
    /// </param>
    /// <returns>
    /// The current <c>TLibrariesDocumentsUrlParams</c> instance for method chaining.
    /// </returns>
    function SortBy(const Value: string = 'created_at'): TLibrariesDocumentsUrlParams;

    /// <summary>
    /// Sets the sort order for the document list.
    /// </summary>
    /// <param name="Value">
    /// The sort order, either <c>"asc"</c> or <c>"desc"</c>. Defaults to <c>"desc"</c>.
    /// </param>
    /// <returns>
    /// The current <c>TLibrariesDocumentsUrlParams</c> instance for method chaining.
    /// </returns>
    function SortOrder(const Value: string = 'desc'): TLibrariesDocumentsUrlParams;
  end;

  /// <summary>
  /// Represents the multipart/form-data parameters used for uploading documents
  /// to a Mistral library.
  /// </summary>
  /// <remarks>
  /// This class provides helper methods to add file streams or file paths
  /// when performing a document upload via the
  /// <c>/v1/libraries/{library_id}/documents</c> endpoint.
  /// </remarks>
  TLibrariesDocumentsUploadParams = class(TMultipartFormData)
    /// <summary>
    /// Adds a file to be uploaded using its file path.
    /// </summary>
    /// <param name="FileName">
    /// The full path to the file on the local file system.
    /// </param>
    /// <returns>
    /// The current <c>TLibrariesDocumentsUploadParams</c> instance for method chaining.
    /// </returns>
    /// <remarks>
    /// The file is sent using the "file" form field in the multipart request.
    /// </remarks>
    function &File(const FileName: string): TLibrariesDocumentsUploadParams; overload;

    /// <summary>
    /// Adds a file to be uploaded using a <c>TStream</c>.
    /// </summary>
    /// <param name="Stream">
    /// The stream containing the file content to be uploaded.
    /// </param>
    /// <param name="FileName">
    /// The name of the file to associate with the stream.
    /// </param>
    /// <returns>
    /// The current <c>TLibrariesDocumentsUploadParams</c> instance for method chaining.
    /// </returns>
    /// <remarks>
    /// This overload allows uploading files that are not stored on disk,
    /// such as dynamically generated files.
    /// </remarks>
    function &File(const Stream: TStream; const FileName: string): TLibrariesDocumentsUploadParams; overload;

    /// <summary>
    /// Initializes a new instance of <c>TLibrariesDocumentsUploadParams</c>
    /// with multipart form-data configuration enabled.
    /// </summary>
    constructor Create; reintroduce;
  end;

  /// <summary>
  /// Represents the JSON body parameters used for updating the metadata
  /// of a document within a Mistral library.
  /// </summary>
  /// <remarks>
  /// This class is primarily used to update the name of an existing document
  /// via the <c>/v1/libraries/{library_id}/documents/{document_id}</c> endpoint.
  /// </remarks>
  TLibrariesDocumentsUpdateParams = class(TJSONParam)
    /// <summary>
    /// Sets a new name for the document.
    /// </summary>
    /// <param name="Value">
    /// The new name to assign to the document.
    /// </param>
    /// <returns>
    /// The current <c>TLibrariesDocumentsUpdateParams</c> instance for method chaining.
    /// </returns>
    /// <remarks>
    /// The name must follow the naming rules of the Mistral API.
    /// </remarks>
    function Name(const Value: string): TLibrariesDocumentsUpdateParams;
  end;

  /// <summary>
  /// Represents pagination metadata for document listings in a Mistral library.
  /// </summary>
  /// <remarks>
  /// This class holds information about the total number of items,
  /// pages, the current page, and other pagination details returned
  /// by the <c>/v1/libraries/{library_id}/documents</c> endpoint.
  /// </remarks>
  TLibrariesPagination = class
  private
    [JsonNameAttribute('total_items')]
    FTotalItems: Int64;
    [JsonNameAttribute('total_pages')]
    FTotalPages: Int64;
    [JsonNameAttribute('current_page')]
    FCurrentPage: Int64;
    [JsonNameAttribute('page_size')]
    FPageSize: Int64;
    [JsonNameAttribute('has_more')]
    FHasMore: Boolean;
  public
    /// <summary>
    /// The total number of items (documents) available in the library.
    /// </summary>
    property TotalItems: Int64 read FTotalItems write FTotalItems;

    /// <summary>
    /// The total number of pages calculated based on <c>TotalItems</c> and <c>PageSize</c>.
    /// </summary>
    property TotalPages: Int64 read FTotalPages write FTotalPages;

    /// <summary>
    /// The current page index, starting from 0.
    /// </summary>
    property CurrentPage: Int64 read FCurrentPage write FCurrentPage;

    /// <summary>
    /// The number of items returned per page.
    /// </summary>
    property PageSize: Int64 read FPageSize write FPageSize;

    /// <summary>
    /// Indicates whether there are more pages to retrieve.
    /// </summary>
    property HasMore: Boolean read FHasMore write FHasMore;
  end;

  /// <summary>
  /// Represents the metadata of a document stored in a Mistral library.
  /// </summary>
  /// <remarks>
  /// This class maps the response structure of the
  /// <c>/v1/libraries/{library_id}/documents</c> endpoint and related document endpoints.
  /// It contains essential information such as file details, upload metadata,
  /// processing status, and token usage for the document.
  /// </remarks>
  TLibrariesDocuments = class(TJSONFingerprint)
  private
    FId: string;
    [JsonNameAttribute('library_id')]
    FLibraryId: string;
    FHash: string;
    [JsonNameAttribute('mime_type')]
    FMimeType: string;
    FExtension: string;
    FSize: Int64;
    FName: string;
    FSummary: string;
    [JsonNameAttribute('created_at')]
    FCreatedAt: string;
    [JsonNameAttribute('last_processed_at')]
    FLastProcessedAt: string;
    [JsonNameAttribute('number_of_pages')]
    FNumberOfPages: Int64;
    [JsonNameAttribute('processing_status')]
    FProcessingStatus: string;
    [JsonNameAttribute('uploaded_by_id')]
    FUploadedById: string;
    [JsonNameAttribute('uploaded_by_type')]
    FUploadedByType: string;
    [JsonNameAttribute('tokens_processing_main_content')]
    FTokensProcessingMainContent: Int64;
    [JsonNameAttribute('tokens_processing_summary')]
    FTokensProcessingSummary: Int64;
    [JsonNameAttribute('tokens_processing_total')]
    FTokensProcessingTotal: Int64;
  public
    /// <summary>
    /// Unique identifier of the document.
    /// </summary>
    property Id: string read FId write FId;

    /// <summary>
    /// Identifier of the library to which the document belongs.
    /// </summary>
    property LibraryId: string read FLibraryId write FLibraryId;

    /// <summary>
    /// Hash value of the document content (used for deduplication).
    /// </summary>
    property Hash: string read FHash write FHash;

    /// <summary>
    /// MIME type of the uploaded file (e.g., application/pdf).
    /// </summary>
    property MimeType: string read FMimeType write FMimeType;

    /// <summary>
    /// File extension of the document (e.g., pdf, docx).
    /// </summary>
    property Extension: string read FExtension write FExtension;

    /// <summary>
    /// Size of the file in bytes.
    /// </summary>
    property Size: Int64 read FSize write FSize;

    /// <summary>
    /// Original or updated name of the document.
    /// </summary>
    property Name: string read FName write FName;

    /// <summary>
    /// Optional summary generated or stored for the document.
    /// </summary>
    property Summary: string read FSummary write FSummary;

    /// <summary>
    /// Timestamp (ISO 8601) indicating when the document was created.
    /// </summary>
    property CreatedAt: string read FCreatedAt write FCreatedAt;

    /// <summary>
    /// Timestamp (ISO 8601) of the last processing completion.
    /// </summary>
    property LastProcessedAt: string read FLastProcessedAt write FLastProcessedAt;

    /// <summary>
    /// Number of pages detected in the document (if applicable).
    /// </summary>
    property NumberOfPages: Int64 read FNumberOfPages write FNumberOfPages;

    /// <summary>
    /// Current processing status of the document
    /// (e.g., queued, processing, completed).
    /// </summary>
    property ProcessingStatus: string read FProcessingStatus write FProcessingStatus;

    /// <summary>
    /// Identifier of the user or entity who uploaded the document.
    /// </summary>
    property UploadedById: string read FUploadedById write FUploadedById;

    /// <summary>
    /// Type of the uploader (e.g., User, Workspace, Org).
    /// </summary>
    property UploadedByType: string read FUploadedByType write FUploadedByType;

    /// <summary>
    /// Number of tokens processed from the main content of the document.
    /// </summary>
    property TokensProcessingMainContent: Int64 read FTokensProcessingMainContent write FTokensProcessingMainContent;

    /// <summary>
    /// Number of tokens processed for the document summary.
    /// </summary>
    property TokensProcessingSummary: Int64 read FTokensProcessingSummary write FTokensProcessingSummary;

    /// <summary>
    /// Total number of tokens processed for the document.
    /// </summary>
    property TokensProcessingTotal: Int64 read FTokensProcessingTotal write FTokensProcessingTotal;
  end;

  /// <summary>
  /// Represents the response structure when listing documents in a Mistral library.
  /// </summary>
  /// <remarks>
  /// This class encapsulates both the pagination information and the array of document metadata
  /// (<c>TLibrariesDocuments</c>) returned by the
  /// <c>/v1/libraries/{library_id}/documents</c> endpoint.
  /// </remarks>
  TLibrariesDocumentsList = class(TJSONFingerprint)
  private
    FPagination: TLibrariesPagination;
    FData: TArray<TLibrariesDocuments>;
  public
    /// <summary>
    /// Contains pagination details such as total items, pages, and current page.
    /// </summary>
    property Pagination: TLibrariesPagination read FPagination write FPagination;

    /// <summary>
    /// The collection of documents (<c>TLibrariesDocuments</c>) in the current page of results.
    /// </summary>
    property Data: TArray<TLibrariesDocuments> read FData write FData;

    /// <summary>
    /// Cleans up resources by freeing the pagination object
    /// and each document instance in the <c>Data</c> array.
    /// </summary>
    destructor Destroy; override;
  end;

  /// <summary>
  /// Represents the response indicating whether a document operation
  /// (such as delete or reprocess) was successfully completed.
  /// </summary>
  /// <remarks>
  /// This class is used by endpoints like
  /// <c>/v1/libraries/{library_id}/documents/{document_id}</c> (DELETE)
  /// and
  /// <c>/v1/libraries/{library_id}/documents/{document_id}/reprocess</c> (POST),
  /// where the API returns a flag indicating the operation status.
  /// </remarks>
  TLibraryDocumentsProcessed = class(TJSONFingerprint)
  private
    FProcessed: Boolean;
  public
    /// <summary>
    /// Indicates whether the document operation was successfully processed.
    /// </summary>
    property Processed: Boolean read FProcessed write FProcessed;
  end;

  /// <summary>
  /// Represents the extracted text content of a document from a Mistral library.
  /// </summary>
  /// <remarks>
  /// This class maps the response of the
  /// <c>/v1/libraries/{library_id}/documents/{document_id}/text_content</c> endpoint.
  /// It contains the raw text extracted by the OCR or document processing pipeline.
  /// </remarks>
  TLibraryDocumentsText = class(TJSONFingerprint)
  private
    FText: string;
  public
    /// <summary>
    /// The full text content extracted from the document.
    /// </summary>
    property Text: string read FText write FText;
  end;

  /// <summary>
  /// Represents the processing status of a document in a Mistral library.
  /// </summary>
  /// <remarks>
  /// This class maps the response of the
  /// <c>/v1/libraries/{library_id}/documents/{document_id}/status</c> endpoint.
  /// It provides information about the document's unique identifier and its current processing state.
  /// </remarks>
  TLibraryDocumentsStatus = class(TJSONFingerprint)
  private
    [JsonNameAttribute('document_id')]
    FDocumentId: string;
    [JsonNameAttribute('processing_status')]
    FProcessingStatus: string;
  public
    /// <summary>
    /// The unique identifier of the document whose status is being queried.
    /// </summary>
    property DocumentId: string read FDocumentId write FDocumentId;

    /// <summary>
    /// The current processing status of the document (e.g., queued, processing, completed).
    /// </summary>
    property ProcessingStatus: string read FProcessingStatus write FProcessingStatus;
  end;

  /// <summary>
  /// Asynchronous callback type for operations returning a <c>TLibrariesDocuments</c> instance.
  /// </summary>
  /// <remarks>
  /// This alias of <c>TAsyncCallback&lt;TLibrariesDocuments&gt;</c> is used for non-blocking
  /// document operations (e.g., retrieve, upload, update) where callbacks such as
  /// <c>OnStart</c>, <c>OnSuccess</c>, and <c>OnError</c> can be assigned to handle the result.
  /// </remarks>
  TAsyncLibrariesDocuments = TAsyncCallback<TLibrariesDocuments>;

  /// <summary>
  /// Promise-based callback type for operations returning a <c>TLibrariesDocuments</c> instance.
  /// </summary>
  /// <remarks>
  /// This alias of <c>TPromiseCallback&lt;TLibrariesDocuments&gt;</c> is used for asynchronous
  /// document operations (e.g., upload, retrieve, update) that follow a promise-style interface.
  /// It enables chaining of success and error handlers, providing a cleaner, event-driven workflow.
  /// </remarks>
  TPromiseLibrariesDocuments = TPromiseCallback<TLibrariesDocuments>;

  /// <summary>
  /// Asynchronous callback type for operations returning a <c>TLibrariesDocumentsList</c> instance.
  /// </summary>
  /// <remarks>
  /// This alias of <c>TAsyncCallback&lt;TLibrariesDocumentsList&gt;</c> is used for non-blocking
  /// document listing operations. It allows attaching event handlers such as <c>OnStart</c>,
  /// <c>OnSuccess</c>, and <c>OnError</c> to manage the response lifecycle when retrieving
  /// paginated lists of documents from a library.
  /// </remarks>
  TAsyncLibrariesDocumentsList = TAsyncCallback<TLibrariesDocumentsList>;

  /// <summary>
  /// Promise-based callback type for operations returning a <c>TLibrariesDocumentsList</c> instance.
  /// </summary>
  /// <remarks>
  /// This alias of <c>TPromiseCallback&lt;TLibrariesDocumentsList&gt;</c> is designed for asynchronous
  /// document listing operations that follow a promise-style pattern.
  /// It enables chaining success and error handlers for handling paginated document retrieval results.
  /// </remarks>
  TPromiseLibrariesDocumentsList = TPromiseCallback<TLibrariesDocumentsList>;

  /// <summary>
  /// Asynchronous callback type for operations returning a <c>TLibraryDocumentsProcessed</c> instance.
  /// </summary>
  /// <remarks>
  /// This alias of <c>TAsyncCallback&lt;TLibraryDocumentsProcessed&gt;</c> is used for non-blocking
  /// operations such as document deletion or reprocessing, where event handlers
  /// (<c>OnStart</c>, <c>OnSuccess</c>, <c>OnError</c>) can be attached to monitor the operation's outcome.
  /// </remarks>
  TAsyncLibraryDocumentsProcessed = TAsyncCallback<TLibraryDocumentsProcessed>;

  /// <summary>
  /// Promise-based callback type for operations returning a <c>TLibraryDocumentsProcessed</c> instance.
  /// </summary>
  /// <remarks>
  /// This alias of <c>TPromiseCallback&lt;TLibraryDocumentsProcessed&gt;</c> is used for asynchronous
  /// operations such as document deletion or reprocessing, following a promise-style interface.
  /// It allows chaining success and error handlers to handle the final result of the operation.
  /// </remarks>
  TPromiseLibraryDocumentsProcessed = TPromiseCallback<TLibraryDocumentsProcessed>;

  /// <summary>
  /// Asynchronous callback type for operations returning a <c>TLibraryDocumentsText</c> instance.
  /// </summary>
  /// <remarks>
  /// This alias of <c>TAsyncCallback&lt;TLibraryDocumentsText&gt;</c> is used for non-blocking
  /// operations that retrieve the extracted text content of a document.
  /// Event handlers such as <c>OnStart</c>, <c>OnSuccess</c>, and <c>OnError</c> can be attached
  /// to monitor the asynchronous text retrieval process.
  /// </remarks>
  TAsyncLibraryDocumentsText = TAsyncCallback<TLibraryDocumentsText>;

  /// <summary>
  /// Promise-based callback type for operations returning a <c>TLibraryDocumentsText</c> instance.
  /// </summary>
  /// <remarks>
  /// This alias of <c>TPromiseCallback&lt;TLibraryDocumentsText&gt;</c> is used for asynchronous
  /// operations that retrieve the text content of a document.
  /// It enables a promise-style workflow, allowing chaining of success and error handlers
  /// to manage the final result of the text extraction process.
  /// </remarks>
  TPromiseLibraryDocumentsText = TPromiseCallback<TLibraryDocumentsText>;

  /// <summary>
  /// Asynchronous callback type for operations returning a <c>TLibraryDocumentsStatus</c> instance.
  /// </summary>
  /// <remarks>
  /// This alias of <c>TAsyncCallback&lt;TLibraryDocumentsStatus&gt;</c> is used for non-blocking
  /// operations that check the processing status of a document.
  /// It allows attaching event handlers (<c>OnStart</c>, <c>OnSuccess</c>, <c>OnError</c>)
  /// to monitor the status retrieval process.
  /// </remarks>
  TAsyncLibraryDocumentsStatus = TAsyncCallback<TLibraryDocumentsStatus>;

  /// <summary>
  /// Promise-based callback type for operations returning a <c>TLibraryDocumentsStatus</c> instance.
  /// </summary>
  /// <remarks>
  /// This alias of <c>TPromiseCallback&lt;TLibraryDocumentsStatus&gt;</c> is used for asynchronous
  /// operations that check the processing status of a document, following a promise-style pattern.
  /// It enables chaining of success and error handlers to handle the final result of the status query.
  /// </remarks>
  TPromiseLibraryDocumentsStatus = TPromiseCallback<TLibraryDocumentsStatus>;

  /// <summary>
  /// Provides a set of methods for managing documents within a specific library
  /// in the Mistral AI API.
  /// </summary>
  /// <remarks>
  /// The <c>TLibrariesDocumentsRoute</c> class offers a structured interface for
  /// performing document-related operations.
  /// <para>It allows listing all documents contained in a library.</para>
  /// <para>It supports uploading new documents with associated metadata.</para>
  /// <para>It enables retrieval of detailed document metadata by ID.</para>
  /// <para>It allows updating properties of a document, such as its name.</para>
  /// <para>It supports deletion of documents and reprocessing for OCR or indexing.</para>
  /// <para>It provides access to extracted text content and document processing status.</para>
  /// <para>
  /// This class serves as the main entry point for working with the
  /// <c>/v1/libraries/{library_id}/documents</c> endpoint group.
  /// </para>
  /// <example>
  /// Example of listing documents in a library:
  /// <code>
  /// var
  ///   DocsList: TLibrariesDocumentsList;
  /// begin
  ///   DocsList := MistralAI.LibrariesDocuments.List('my_library_id',
  ///     procedure(Params: TLibrariesDocumentsUrlParams)
  ///     begin
  ///       Params.Limit(10).Order('asc');
  ///     end
  ///   );
  /// end;
  /// </code>
  /// </example>
  /// </remarks>
  TLibrariesDocumentsRoute = class(TMistralAIAPIRoute)
    /// <summary>
    /// Asynchronously retrieves a paginated list of documents from a specified library using a promise-based interface.
    /// </summary>
    /// <param name="LibraryId">
    /// The unique identifier of the library from which to list documents.
    /// </param>
    /// <param name="ParamProc">
    /// A procedure to configure <c>TLibrariesDocumentsUrlParams</c>,
    /// such as search filters, pagination options, and sorting criteria.
    /// </param>
    /// <param name="Callbacks">
    /// An optional function returning a <c>TPromiseLibrariesDocumentsList</c> instance
    /// to chain success and error handlers for the asynchronous operation.
    /// </param>
    /// <returns>
    /// A <c>TPromise&lt;TLibrariesDocumentsList&gt;</c> that resolves with a paginated list of documents
    /// or rejects with an error if the operation fails.
    /// </returns>
    /// <remarks>
    /// This method does not block the main thread and follows a promise-based pattern.
    /// Use <c>List</c> for a synchronous version or <c>AsyncList</c> for a callback-based asynchronous approach.
    /// </remarks>
    function AsyncAwaitList(const LibraryId: string;
      const ParamProc: TProc<TLibrariesDocumentsUrlParams>;
      const Callbacks: TFunc<TPromiseLibrariesDocumentsList> = nil): TPromise<TLibrariesDocumentsList>;

    /// <summary>
    /// Asynchronously uploads a document to a specified library using a promise-based interface.
    /// </summary>
    /// <param name="LibraryId">
    /// The unique identifier of the library where the document will be uploaded.
    /// </param>
    /// <param name="ParamProc">
    /// A procedure to configure the <c>TLibrariesDocumentsUploadParams</c>,
    /// such as specifying the file path or stream for the document to upload.
    /// </param>
    /// <param name="Callbacks">
    /// An optional function returning a <c>TPromiseLibrariesDocuments</c> instance
    /// to attach success and error handlers for the asynchronous upload.
    /// </param>
    /// <returns>
    /// A <c>TPromise&lt;TLibrariesDocuments&gt;</c> that resolves with the metadata
    /// of the uploaded document or rejects with an error if the upload fails.
    /// </returns>
    /// <remarks>
    /// This method performs the upload without blocking the main thread.
    /// Use <c>Upload</c> for a synchronous version or <c>AsyncUpload</c> for a callback-based asynchronous approach.
    /// </remarks>
    function AsyncAwaitUpload(const LibraryId: string;
      const ParamProc: TProc<TLibrariesDocumentsUploadParams>;
      const Callbacks: TFunc<TPromiseLibrariesDocuments> = nil): TPromise<TLibrariesDocuments>;

    /// <summary>
    /// Asynchronously retrieves the metadata of a specific document in a given library using a promise-based interface.
    /// </summary>
    /// <param name="LibraryId">
    /// The unique identifier of the library containing the document.
    /// </param>
    /// <param name="DocumentId">
    /// The unique identifier of the document to retrieve.
    /// </param>
    /// <param name="Callbacks">
    /// An optional function returning a <c>TPromiseLibrariesDocuments</c> instance
    /// to attach success and error handlers for the asynchronous retrieval.
    /// </param>
    /// <returns>
    /// A <c>TPromise&lt;TLibrariesDocuments&gt;</c> that resolves with the document's metadata
    /// or rejects with an error if the retrieval fails.
    /// </returns>
    /// <remarks>
    /// This method does not block the main thread.
    /// Use <c>Retrieve</c> for a synchronous version or <c>AsyncRetrieve</c> for a callback-based asynchronous approach.
    /// </remarks>
    function AsyncAwaitRetrieve(const LibraryId: string;
      const DocumentId: string;
      const Callbacks: TFunc<TPromiseLibrariesDocuments> = nil): TPromise<TLibrariesDocuments>;

    /// <summary>
    /// Asynchronously updates the metadata of a specific document in a given library using a promise-based interface.
    /// </summary>
    /// <param name="LibraryId">
    /// The unique identifier of the library containing the document.
    /// </param>
    /// <param name="DocumentId">
    /// The unique identifier of the document to update.
    /// </param>
    /// <param name="ParamProc">
    /// A procedure to configure the <c>TLibrariesDocumentsUpdateParams</c>,
    /// such as setting a new document name.
    /// </param>
    /// <param name="Callbacks">
    /// An optional function returning a <c>TPromiseLibrariesDocuments</c> instance
    /// to attach success and error handlers for the asynchronous update.
    /// </param>
    /// <returns>
    /// A <c>TPromise&lt;TLibrariesDocuments&gt;</c> that resolves with the updated document metadata
    /// or rejects with an error if the update fails.
    /// </returns>
    /// <remarks>
    /// This method executes the update operation without blocking the main thread.
    /// Use <c>Update</c> for a synchronous version or <c>AsyncUpdate</c> for a callback-based asynchronous approach.
    /// </remarks>
    function AsyncAwaitUpdate(const LibraryId: string;
      const DocumentId: string;
      const ParamProc: TProc<TLibrariesDocumentsUpdateParams>;
      const Callbacks: TFunc<TPromiseLibrariesDocuments> = nil): TPromise<TLibrariesDocuments>;

    /// <summary>
    /// Asynchronously deletes a specific document from the given library using a promise-based interface.
    /// </summary>
    /// <param name="LibraryId">
    /// The unique identifier of the library containing the document to delete.
    /// </param>
    /// <param name="DocumentId">
    /// The unique identifier of the document to delete.
    /// </param>
    /// <param name="Callbacks">
    /// An optional function returning a <c>TPromiseLibraryDocumentsProcessed</c> instance
    /// to attach success and error handlers for the asynchronous deletion.
    /// </param>
    /// <returns>
    /// A <c>TPromise&lt;TLibraryDocumentsProcessed&gt;</c> that resolves when the document is successfully deleted
    /// or rejects with an error if the operation fails.
    /// </returns>
    /// <remarks>
    /// This method triggers the deletion without blocking the main thread.
    /// Use <c>Delete</c> for a synchronous version or <c>AsyncDelete</c> for a callback-based asynchronous approach.
    /// </remarks>
    function AsyncAwaitDelete(const LibraryId: string;
      const DocumentId: string;
      const Callbacks: TFunc<TPromiseLibraryDocumentsProcessed> = nil): TPromise<TLibraryDocumentsProcessed>;

    /// <summary>
    /// Asynchronously retrieves the extracted text content of a specific document from a given library using a promise-based interface.
    /// </summary>
    /// <param name="LibraryId">
    /// The unique identifier of the library containing the document.
    /// </param>
    /// <param name="DocumentId">
    /// The unique identifier of the document from which to retrieve text content.
    /// </param>
    /// <param name="Callbacks">
    /// An optional function returning a <c>TPromiseLibraryDocumentsText</c> instance
    /// to attach success and error handlers for the asynchronous text retrieval.
    /// </param>
    /// <returns>
    /// A <c>TPromise&lt;TLibraryDocumentsText&gt;</c> that resolves with the extracted text content
    /// or rejects with an error if the retrieval fails.
    /// </returns>
    /// <remarks>
    /// This method performs text retrieval without blocking the main thread.
    /// Use <c>RetrieveText</c> for a synchronous version or <c>AsyncRetrieveText</c> for a callback-based asynchronous approach.
    /// </remarks>
    function AsyncAwaitRetrieveText(const LibraryId: string;
      const DocumentId: string;
      const Callbacks: TFunc<TPromiseLibraryDocumentsText> = nil): TPromise<TLibraryDocumentsText>;

    /// <summary>
    /// Asynchronously retrieves the processing status of a specific document from a given library using a promise-based interface.
    /// </summary>
    /// <param name="LibraryId">
    /// The unique identifier of the library containing the document.
    /// </param>
    /// <param name="DocumentId">
    /// The unique identifier of the document for which to retrieve the processing status.
    /// </param>
    /// <param name="Callbacks">
    /// An optional function returning a <c>TPromiseLibraryDocumentsStatus</c> instance
    /// to attach success and error handlers for the asynchronous status retrieval.
    /// </param>
    /// <returns>
    /// A <c>TPromise&lt;TLibraryDocumentsStatus&gt;</c> that resolves with the document's current processing status
    /// or rejects with an error if the retrieval fails.
    /// </returns>
    /// <remarks>
    /// This method retrieves the document status without blocking the main thread.
    /// Use <c>RetrieveStatus</c> for a synchronous version or <c>AsyncRetrieveStatus</c> for a callback-based asynchronous approach.
    /// </remarks>
    function AsyncAwaitRetrieveStatus(const LibraryId: string;
      const DocumentId: string;
      const Callbacks: TFunc<TPromiseLibraryDocumentsStatus> = nil): TPromise<TLibraryDocumentsStatus>;

    /// <summary>
    /// Asynchronously retrieves a temporary signed URL to access a specific document in a given library using a promise-based interface.
    /// </summary>
    /// <param name="LibraryId">
    /// The unique identifier of the library containing the document.
    /// </param>
    /// <param name="DocumentId">
    /// The unique identifier of the document for which to generate the signed URL.
    /// </param>
    /// <param name="Callbacks">
    /// An optional function returning a <c>TPromiseSignedUrl</c> instance
    /// to attach success and error handlers for the asynchronous signed URL retrieval.
    /// </param>
    /// <returns>
    /// A <c>TPromise&lt;TSignedUrl&gt;</c> that resolves with the signed URL
    /// or rejects with an error if the URL generation fails.
    /// </returns>
    /// <remarks>
    /// The signed URL is valid for a limited time (typically 30 minutes).
    /// Use <c>GetDocumentSignedUrl</c> for a synchronous version
    /// or <c>AsyncGetDocumentSignedUrl</c> for a callback-based asynchronous approach.
    /// </remarks>
    function AsyncAwaitGetDocumentSignedUrl(const LibraryId: string;
      const DocumentId: string;
      const Callbacks: TFunc<TPromiseSignedUrl> = nil): TPromise<TSignedUrl>;

    /// <summary>
    /// Asynchronously retrieves a temporary signed URL to access the extracted text of a specific document
    /// in a given library using a promise-based interface.
    /// </summary>
    /// <param name="LibraryId">
    /// The unique identifier of the library containing the document.
    /// </param>
    /// <param name="DocumentId">
    /// The unique identifier of the document for which to generate the signed text URL.
    /// </param>
    /// <param name="Callbacks">
    /// An optional function returning a <c>TPromiseSignedUrl</c> instance
    /// to attach success and error handlers for the asynchronous signed URL retrieval.
    /// </param>
    /// <returns>
    /// A <c>TPromise&lt;TSignedUrl&gt;</c> that resolves with the signed URL
    /// to access the document's extracted text or rejects with an error if the operation fails.
    /// </returns>
    /// <remarks>
    /// The signed URL is valid for a limited time (typically 30 minutes).
    /// Use <c>GetTextSignedUrl</c> for a synchronous version
    /// or <c>AsyncGetTextSignedUrl</c> for a callback-based asynchronous approach.
    /// </remarks>
    function AsyncAwaitGetTextSignedUrl(const LibraryId: string;
      const DocumentId: string;
      const Callbacks: TFunc<TPromiseSignedUrl> = nil): TPromise<TSignedUrl>;

    /// <summary>
    /// Asynchronously triggers the reprocessing of a specific document in a given library using a promise-based interface.
    /// </summary>
    /// <param name="LibraryId">
    /// The unique identifier of the library containing the document.
    /// </param>
    /// <param name="DocumentId">
    /// The unique identifier of the document to reprocess.
    /// </param>
    /// <param name="Callbacks">
    /// An optional function returning a <c>TPromiseLibraryDocumentsProcessed</c> instance
    /// to attach success and error handlers for the asynchronous reprocess operation.
    /// </param>
    /// <returns>
    /// A <c>TPromise&lt;TLibraryDocumentsProcessed&gt;</c> that resolves when the reprocess operation is successfully triggered
    /// or rejects with an error if the request fails.
    /// </returns>
    /// <remarks>
    /// This method initiates the reprocess operation without blocking the main thread.
    /// Use <c>Reprocess</c> for a synchronous version or <c>AsyncReprocess</c> for a callback-based asynchronous approach.
    /// </remarks>
    function AsyncAwaitReprocess(const LibraryId: string;
      const DocumentId: string;
      const Callbacks: TFunc<TPromiseLibraryDocumentsProcessed> = nil): TPromise<TLibraryDocumentsProcessed>;

    /// <summary>
    /// Retrieves a paginated list of documents from a specified library.
    /// </summary>
    /// <param name="LibraryId">
    /// The unique identifier of the library from which to list documents.
    /// </param>
    /// <param name="ParamProc">
    /// A procedure to configure the <c>TLibrariesDocumentsUrlParams</c>
    /// (e.g., search filters, pagination, sorting options).
    /// </param>
    /// <returns>
    /// A <c>TLibrariesDocumentsList</c> instance containing the pagination information
    /// and an array of <c>TLibrariesDocuments</c> objects representing the listed documents.
    /// </returns>
    /// <remarks>
    /// This is a synchronous call that blocks until the list of documents is retrieved
    /// or an exception occurs.
    /// Use the asynchronous counterpart (<c>AsyncList</c> or <c>AsyncAwaitList</c>)
    /// for non-blocking operations.
    /// </remarks>
    function List(const LibraryId: string;
      const ParamProc: TProc<TLibrariesDocumentsUrlParams>): TLibrariesDocumentsList;

    /// <summary>
    /// Uploads a new document to the specified library.
    /// </summary>
    /// <param name="LibraryId">
    /// The unique identifier of the target library where the document will be uploaded.
    /// </param>
    /// <param name="ParamProc">
    /// A procedure to configure the <c>TLibrariesDocumentsUploadParams</c>,
    /// including the file path or stream of the document to upload.
    /// </param>
    /// <returns>
    /// A <c>TLibrariesDocuments</c> instance containing the metadata of the uploaded document.
    /// </returns>
    /// <remarks>
    /// This is a synchronous operation and will block until the upload completes or an error occurs.
    /// If a document with the same hash already exists in the library,
    /// the existing document information is returned instead of creating a new one.
    /// Use the asynchronous counterparts (<c>AsyncUpload</c> or <c>AsyncAwaitUpload</c>) for non-blocking workflows.
    /// </remarks>
    function Upload(const LibraryId: string;
      const ParamProc: TProc<TLibrariesDocumentsUploadParams>): TLibrariesDocuments;

    /// <summary>
    /// Retrieves the metadata of a specific document within a given library.
    /// </summary>
    /// <param name="LibraryId">
    /// The unique identifier of the library containing the document.
    /// </param>
    /// <param name="DocumentId">
    /// The unique identifier of the document to retrieve.
    /// </param>
    /// <returns>
    /// A <c>TLibrariesDocuments</c> instance containing the full metadata of the specified document.
    /// </returns>
    /// <remarks>
    /// This is a synchronous operation and will block until the document metadata is fully retrieved.
    /// Use the asynchronous counterparts (<c>AsyncRetrieve</c> or <c>AsyncAwaitRetrieve</c>)
    /// for non-blocking retrieval of document information.
    /// </remarks>
    function Retrieve(const LibraryId: string;
      const DocumentId: string): TLibrariesDocuments;

    /// <summary>
    /// Updates the metadata of a specific document in a given library.
    /// </summary>
    /// <param name="LibraryId">
    /// The unique identifier of the library containing the document.
    /// </param>
    /// <param name="DocumentId">
    /// The unique identifier of the document to update.
    /// </param>
    /// <param name="ParamProc">
    /// A procedure to configure the <c>TLibrariesDocumentsUpdateParams</c>,
    /// such as setting a new document name.
    /// </param>
    /// <returns>
    /// A <c>TLibrariesDocuments</c> instance containing the updated metadata of the document.
    /// </returns>
    /// <remarks>
    /// This is a synchronous operation that blocks until the update is applied.
    /// Use the asynchronous counterparts (<c>AsyncUpdate</c> or <c>AsyncAwaitUpdate</c>)
    /// for non-blocking updates.
    /// </remarks>
    function Update(const LibraryId: string;
      const DocumentId: string;
      const ParamProc: TProc<TLibrariesDocumentsUpdateParams>): TLibrariesDocuments;

    /// <summary>
    /// Deletes a specific document from the given library.
    /// </summary>
    /// <param name="LibraryId">
    /// The unique identifier of the library containing the document.
    /// </param>
    /// <param name="DocumentId">
    /// The unique identifier of the document to delete.
    /// </param>
    /// <returns>
    /// A <c>TLibraryDocumentsProcessed</c> instance indicating whether the deletion was successfully processed.
    /// </returns>
    /// <remarks>
    /// This is a synchronous operation that blocks until the document is removed from both the library
    /// and the search index.
    /// Use the asynchronous counterparts (<c>AsyncDelete</c> or <c>AsyncAwaitDelete</c>)
    /// for non-blocking document deletion.
    /// </remarks>
    function Delete(const LibraryId: string;
      const DocumentId: string): TLibraryDocumentsProcessed;

    /// <summary>
    /// Retrieves the extracted text content of a specific document in a given library.
    /// </summary>
    /// <param name="LibraryId">
    /// The unique identifier of the library containing the document.
    /// </param>
    /// <param name="DocumentId">
    /// The unique identifier of the document from which to extract text.
    /// </param>
    /// <returns>
    /// A <c>TLibraryDocumentsText</c> instance containing the extracted text content of the document.
    /// </returns>
    /// <remarks>
    /// This is a synchronous operation that blocks until the text content is fully retrieved.
    /// Use the asynchronous counterparts (<c>AsyncRetrieveText</c> or <c>AsyncAwaitRetrieveText</c>)
    /// for non-blocking text retrieval.
    /// </remarks>
    function RetrieveText(const LibraryId: string;
      const DocumentId: string): TLibraryDocumentsText;

    /// <summary>
    /// Retrieves the current processing status of a specific document in a given library.
    /// </summary>
    /// <param name="LibraryId">
    /// The unique identifier of the library containing the document.
    /// </param>
    /// <param name="DocumentId">
    /// The unique identifier of the document for which to retrieve the processing status.
    /// </param>
    /// <returns>
    /// A <c>TLibraryDocumentsStatus</c> instance containing the document ID and its current processing status.
    /// </returns>
    /// <remarks>
    /// This is a synchronous operation that blocks until the status information is retrieved.
    /// Use the asynchronous counterparts (<c>AsyncRetrieveStatus</c> or <c>AsyncAwaitRetrieveStatus</c>)
    /// for non-blocking status checks.
    /// </remarks>
    function RetrieveStatus(const LibraryId: string;
      const DocumentId: string): TLibraryDocumentsStatus;

    /// <summary>
    /// Retrieves a temporary signed URL to access the specified document in a library.
    /// </summary>
    /// <param name="LibraryId">
    /// The unique identifier of the library containing the document.
    /// </param>
    /// <param name="DocumentId">
    /// The unique identifier of the document for which to generate a signed URL.
    /// </param>
    /// <returns>
    /// A <c>TSignedUrl</c> instance containing a signed URL that can be used to download the document.
    /// </returns>
    /// <remarks>
    /// The signed URL is valid for a limited time (typically 30 minutes) and can be accessed by anyone with the link.
    /// This is a synchronous call; use <c>AsyncGetDocumentSignedUrl</c> or <c>AsyncAwaitGetDocumentSignedUrl</c>
    /// for non-blocking operations.
    /// </remarks>
    function GetDocumentSignedUrl(const LibraryId: string;
      const DocumentId: string): TSignedUrl;

    /// <summary>
    /// Retrieves a temporary signed URL for accessing the extracted text of a specific document in a library.
    /// </summary>
    /// <param name="LibraryId">
    /// The unique identifier of the library containing the document.
    /// </param>
    /// <param name="DocumentId">
    /// The unique identifier of the document for which to generate a signed text URL.
    /// </param>
    /// <returns>
    /// A <c>TSignedUrl</c> instance containing a signed URL to download or access the extracted text content.
    /// </returns>
    /// <remarks>
    /// The signed URL is valid for a limited duration (typically 30 minutes) and can be accessed by anyone with the link.
    /// This is a synchronous call; use <c>AsyncGetTextSignedUrl</c> or <c>AsyncAwaitGetTextSignedUrl</c>
    /// for non-blocking retrieval of text URLs.
    /// </remarks>
    function GetTextSignedUrl(const LibraryId: string;
      const DocumentId: string): TSignedUrl;

    /// <summary>
    /// Reprocesses a specific document in the given library.
    /// </summary>
    /// <param name="LibraryId">
    /// The unique identifier of the library containing the document to be reprocessed.
    /// </param>
    /// <param name="DocumentId">
    /// The unique identifier of the document to reprocess.
    /// </param>
    /// <returns>
    /// A <c>TLibraryDocumentsProcessed</c> instance indicating whether the reprocessing was successfully initiated.
    /// </returns>
    /// <remarks>
    /// This is a synchronous call that blocks until the reprocess request is sent.
    /// The operation may incur additional costs as it triggers a new processing cycle for the document.
    /// Use the asynchronous counterparts (<c>AsyncReprocess</c> or <c>AsyncAwaitReprocess</c>)
    /// for non-blocking reprocessing.
    /// </remarks>
    function Reprocess(const LibraryId: string;
      const DocumentId: string): TLibraryDocumentsProcessed;

    /// <summary>
    /// Asynchronously retrieves a paginated list of documents from a specified library.
    /// </summary>
    /// <param name="LibraryId">
    /// The unique identifier of the library from which to list documents.
    /// </param>
    /// <param name="ParamProc">
    /// A procedure to configure the <c>TLibrariesDocumentsUrlParams</c>
    /// (e.g., search filters, pagination, sorting options).
    /// </param>
    /// <param name="Callbacks">
    /// A function returning a <c>TAsyncLibrariesDocumentsList</c> instance
    /// with event handlers (<c>OnStart</c>, <c>OnSuccess</c>, <c>OnError</c>)
    /// to handle the asynchronous operation lifecycle.
    /// </param>
    /// <remarks>
    /// This method executes the document listing operation without blocking the main thread.
    /// Use <c>AsyncAwaitList</c> for a promise-based asynchronous pattern or <c>List</c> for the synchronous version.
    /// </remarks>
    procedure AsyncList(const LibraryId: string;
      const ParamProc: TProc<TLibrariesDocumentsUrlParams>;
      const Callbacks: TFunc<TAsyncLibrariesDocumentsList>);

    /// <summary>
    /// Asynchronously uploads a new document to the specified library.
    /// </summary>
    /// <param name="LibraryId">
    /// The unique identifier of the library where the document will be uploaded.
    /// </param>
    /// <param name="ParamProc">
    /// A procedure to configure the <c>TLibrariesDocumentsUploadParams</c>,
    /// such as specifying the file path or stream to upload.
    /// </param>
    /// <param name="Callbacks">
    /// A function returning a <c>TAsyncLibrariesDocuments</c> instance
    /// with event handlers (<c>OnStart</c>, <c>OnSuccess</c>, <c>OnError</c>)
    /// to manage the lifecycle of the asynchronous upload operation.
    /// </param>
    /// <remarks>
    /// This method performs the upload operation without blocking the main thread.
    /// Use <c>AsyncAwaitUpload</c> for a promise-based workflow or <c>Upload</c> for the synchronous version.
    /// </remarks>
    procedure AsyncUpload(const LibraryId: string;
      const ParamProc: TProc<TLibrariesDocumentsUploadParams>;
      const Callbacks: TFunc<TAsyncLibrariesDocuments>);

    /// <summary>
    /// Asynchronously retrieves the metadata of a specific document in a given library.
    /// </summary>
    /// <param name="LibraryId">
    /// The unique identifier of the library containing the document.
    /// </param>
    /// <param name="DocumentId">
    /// The unique identifier of the document to retrieve.
    /// </param>
    /// <param name="Callbacks">
    /// A function returning a <c>TAsyncLibrariesDocuments</c> instance
    /// with event handlers (<c>OnStart</c>, <c>OnSuccess</c>, <c>OnError</c>)
    /// to handle the asynchronous retrieval process.
    /// </param>
    /// <remarks>
    /// This method performs the retrieval operation without blocking the main thread.
    /// Use <c>AsyncAwaitRetrieve</c> for a promise-based approach or <c>Retrieve</c> for the synchronous version.
    /// </remarks>
    procedure AsyncRetrieve(const LibraryId: string;
      const DocumentId: string;
      const Callbacks: TFunc<TAsyncLibrariesDocuments>);

    /// <summary>
    /// Asynchronously updates the metadata of a specific document in a given library.
    /// </summary>
    /// <param name="LibraryId">
    /// The unique identifier of the library containing the document.
    /// </param>
    /// <param name="DocumentId">
    /// The unique identifier of the document to update.
    /// </param>
    /// <param name="ParamProc">
    /// A procedure to configure the <c>TLibrariesDocumentsUpdateParams</c>,
    /// for example, to set a new document name.
    /// </param>
    /// <param name="Callbacks">
    /// A function returning a <c>TAsyncLibrariesDocuments</c> instance
    /// with event handlers (<c>OnStart</c>, <c>OnSuccess</c>, <c>OnError</c>)
    /// to manage the asynchronous update operation.
    /// </param>
    /// <remarks>
    /// This method executes the update without blocking the main thread.
    /// Use <c>AsyncAwaitUpdate</c> for a promise-based approach or <c>Update</c> for the synchronous version.
    /// </remarks>
    procedure AsyncUpdate(const LibraryId: string;
      const DocumentId: string;
      const ParamProc: TProc<TLibrariesDocumentsUpdateParams>;
      const Callbacks: TFunc<TAsyncLibrariesDocuments>);

    /// <summary>
    /// Asynchronously deletes a specific document from the given library.
    /// </summary>
    /// <param name="LibraryId">
    /// The unique identifier of the library containing the document to delete.
    /// </param>
    /// <param name="DocumentId">
    /// The unique identifier of the document to delete.
    /// </param>
    /// <param name="Callbacks">
    /// A function returning a <c>TAsyncLibraryDocumentsProcessed</c> instance
    /// with event handlers (<c>OnStart</c>, <c>OnSuccess</c>, <c>OnError</c>)
    /// to monitor the asynchronous deletion operation.
    /// </param>
    /// <remarks>
    /// This method performs the delete operation without blocking the main thread.
    /// Use <c>AsyncAwaitDelete</c> for a promise-based approach or <c>Delete</c> for the synchronous version.
    /// </remarks>
    procedure AsyncDelete(const LibraryId: string;
      const DocumentId: string;
      const Callbacks: TFunc<TAsyncLibraryDocumentsProcessed>);

    /// <summary>
    /// Asynchronously retrieves the extracted text content of a specific document from a given library.
    /// </summary>
    /// <param name="LibraryId">
    /// The unique identifier of the library containing the document.
    /// </param>
    /// <param name="DocumentId">
    /// The unique identifier of the document from which to retrieve text content.
    /// </param>
    /// <param name="Callbacks">
    /// A function returning a <c>TAsyncLibraryDocumentsText</c> instance
    /// with event handlers (<c>OnStart</c>, <c>OnSuccess</c>, <c>OnError</c>)
    /// to manage the asynchronous text retrieval process.
    /// </param>
    /// <remarks>
    /// This method retrieves text without blocking the main thread.
    /// Use <c>AsyncAwaitRetrieveText</c> for a promise-based approach or <c>RetrieveText</c> for the synchronous version.
    /// </remarks>
    procedure AsyncRetrieveText(const LibraryId: string;
      const DocumentId: string;
      const Callbacks: TFunc<TAsyncLibraryDocumentsText>);

    /// <summary>
    /// Asynchronously retrieves the processing status of a specific document from a given library.
    /// </summary>
    /// <param name="LibraryId">
    /// The unique identifier of the library containing the document.
    /// </param>
    /// <param name="DocumentId">
    /// The unique identifier of the document for which to retrieve the processing status.
    /// </param>
    /// <param name="Callbacks">
    /// A function returning a <c>TAsyncLibraryDocumentsStatus</c> instance
    /// with event handlers (<c>OnStart</c>, <c>OnSuccess</c>, <c>OnError</c>)
    /// to manage the asynchronous status retrieval process.
    /// </param>
    /// <remarks>
    /// This method retrieves the document status without blocking the main thread.
    /// Use <c>AsyncAwaitRetrieveStatus</c> for a promise-based approach or <c>RetrieveStatus</c> for the synchronous version.
    /// </remarks>
    procedure AsyncRetrieveStatus(const LibraryId: string;
      const DocumentId: string;
      const Callbacks: TFunc<TAsyncLibraryDocumentsStatus>);

    /// <summary>
    /// Asynchronously retrieves a temporary signed URL to access a specific document in a given library.
    /// </summary>
    /// <param name="LibraryId">
    /// The unique identifier of the library containing the document.
    /// </param>
    /// <param name="DocumentId">
    /// The unique identifier of the document for which to generate the signed URL.
    /// </param>
    /// <param name="Callbacks">
    /// A function returning a <c>TAsyncSignedUrl</c> instance
    /// with event handlers (<c>OnStart</c>, <c>OnSuccess</c>, <c>OnError</c>)
    /// to manage the asynchronous URL generation process.
    /// </param>
    /// <remarks>
    /// The generated signed URL is valid for a limited time (typically 30 minutes).
    /// Use <c>AsyncAwaitGetDocumentSignedUrl</c> for a promise-based workflow or <c>GetDocumentSignedUrl</c> for the synchronous version.
    /// </remarks>
    procedure AsyncGetDocumentSignedUrl(const LibraryId: string;
      const DocumentId: string;
      const Callbacks: TFunc<TAsyncSignedUrl>);

    /// <summary>
    /// Asynchronously retrieves a temporary signed URL to access the extracted text of a specific document in a given library.
    /// </summary>
    /// <param name="LibraryId">
    /// The unique identifier of the library containing the document.
    /// </param>
    /// <param name="DocumentId">
    /// The unique identifier of the document for which to generate the signed text URL.
    /// </param>
    /// <param name="Callbacks">
    /// A function returning a <c>TAsyncSignedUrl</c> instance
    /// with event handlers (<c>OnStart</c>, <c>OnSuccess</c>, <c>OnError</c>)
    /// to manage the asynchronous URL generation process.
    /// </param>
    /// <remarks>
    /// The signed URL is valid for a limited time (typically 30 minutes).
    /// Use <c>AsyncAwaitGetTextSignedUrl</c> for a promise-based workflow
    /// or <c>GetTextSignedUrl</c> for the synchronous version.
    /// </remarks>
    procedure AsyncGetTextSignedUrl(const LibraryId: string;
      const DocumentId: string;
      const Callbacks: TFunc<TAsyncSignedUrl>);

    /// <summary>
    /// Asynchronously triggers reprocessing of a specific document in the given library.
    /// </summary>
    /// <param name="LibraryId">
    /// The unique identifier of the library containing the document.
    /// </param>
    /// <param name="DocumentId">
    /// The unique identifier of the document to reprocess.
    /// </param>
    /// <param name="Callbacks">
    /// A function returning a <c>TAsyncLibraryDocumentsProcessed</c> instance
    /// with event handlers (<c>OnStart</c>, <c>OnSuccess</c>, <c>OnError</c>)
    /// to monitor the asynchronous reprocessing operation.
    /// </param>
    /// <remarks>
    /// This method performs the reprocess operation without blocking the main thread.
    /// Use <c>AsyncAwaitReprocess</c> for a promise-based approach
    /// or <c>Reprocess</c> for the synchronous version.
    /// </remarks>
    procedure AsyncReprocess(const LibraryId: string;
      const DocumentId: string;
      const Callbacks: TFunc<TAsyncLibraryDocumentsProcessed>);
  end;

implementation

{ TLibrariesDocumentsUrlParams }

function TLibrariesDocumentsUrlParams.Page(
  const Value: Integer): TLibrariesDocumentsUrlParams;
begin
  Result := TLibrariesDocumentsUrlParams(Add('page', Value));
end;

function TLibrariesDocumentsUrlParams.PageSize(
  const Value: Integer): TLibrariesDocumentsUrlParams;
begin
  Result := TLibrariesDocumentsUrlParams(Add('page_size', Value));
end;

function TLibrariesDocumentsUrlParams.Search(
  const Value: string): TLibrariesDocumentsUrlParams;
begin
  Result := TLibrariesDocumentsUrlParams(Add('search', Value));
end;

function TLibrariesDocumentsUrlParams.SortBy(
  const Value: string): TLibrariesDocumentsUrlParams;
begin
  Result := TLibrariesDocumentsUrlParams(Add('sort_by', Value));
end;

function TLibrariesDocumentsUrlParams.SortOrder(
  const Value: string): TLibrariesDocumentsUrlParams;
begin
  Result := TLibrariesDocumentsUrlParams(Add('sort_order', Value));
end;

{ TLibrariesDocumentsUploadParams }

function TLibrariesDocumentsUploadParams.&File(
  const FileName: string): TLibrariesDocumentsUploadParams;
begin
  AddFile('file', FileName);
  Result := Self;
end;

constructor TLibrariesDocumentsUploadParams.Create;
begin
  inherited Create(True);
end;

function TLibrariesDocumentsUploadParams.&File(const Stream: TStream;
  const FileName: string): TLibrariesDocumentsUploadParams;
begin
  {$IF RTLVersion > 35.0}
  AddStream('file', Stream, True, FileName);
  {$ELSE}
  AddStream('file', Stream, FileName);
  {$ENDIF}
  Result := Self;
end;

{ TLibrariesDocumentsUpdateParams }

function TLibrariesDocumentsUpdateParams.Name(
  const Value: string): TLibrariesDocumentsUpdateParams;
begin
  Result := TLibrariesDocumentsUpdateParams(Add('name', Value));
end;

{ TLibrariesDocumentsList }

destructor TLibrariesDocumentsList.Destroy;
begin
  if Assigned(FPagination) then
    FPagination.Free;
  for var Item in FData do
    Item.Free;
  inherited;
end;

{ TLibrariesDocumentsRoute }

function TLibrariesDocumentsRoute.AsyncAwaitDelete(const LibraryId,
  DocumentId: string;
  const Callbacks: TFunc<TPromiseLibraryDocumentsProcessed>): TPromise<TLibraryDocumentsProcessed>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TLibraryDocumentsProcessed>(
    procedure(const CallbackParams: TFunc<TAsyncLibraryDocumentsProcessed>)
    begin
      AsyncDelete(LibraryId, DocumentId, CallbackParams);
    end,
    Callbacks);
end;

function TLibrariesDocumentsRoute.AsyncAwaitGetDocumentSignedUrl(
  const LibraryId, DocumentId: string;
  const Callbacks: TFunc<TPromiseSignedUrl>): TPromise<TSignedUrl>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TSignedUrl>(
    procedure(const CallbackParams: TFunc<TAsyncSignedUrl>)
    begin
      AsyncGetDocumentSignedUrl(LibraryId, DocumentId, CallbackParams);
    end,
    Callbacks);
end;

function TLibrariesDocumentsRoute.AsyncAwaitGetTextSignedUrl(const LibraryId,
  DocumentId: string;
  const Callbacks: TFunc<TPromiseSignedUrl>): TPromise<TSignedUrl>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TSignedUrl>(
    procedure(const CallbackParams: TFunc<TAsyncSignedUrl>)
    begin
      AsyncGetTextSignedUrl(LibraryId, DocumentId, CallbackParams);
    end,
    Callbacks);
end;

function TLibrariesDocumentsRoute.AsyncAwaitList(const LibraryId: string;
  const ParamProc: TProc<TLibrariesDocumentsUrlParams>;
  const Callbacks: TFunc<TPromiseLibrariesDocumentsList>): TPromise<TLibrariesDocumentsList>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TLibrariesDocumentsList>(
    procedure(const CallbackParams: TFunc<TAsyncLibrariesDocumentsList>)
    begin
      AsyncList(LibraryId, ParamProc, CallbackParams);
    end,
    Callbacks);
end;

function TLibrariesDocumentsRoute.AsyncAwaitReprocess(const LibraryId,
  DocumentId: string;
  const Callbacks: TFunc<TPromiseLibraryDocumentsProcessed>): TPromise<TLibraryDocumentsProcessed>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TLibraryDocumentsProcessed>(
    procedure(const CallbackParams: TFunc<TAsyncLibraryDocumentsProcessed>)
    begin
      AsyncReprocess(LibraryId, DocumentId, CallbackParams);
    end,
    Callbacks);
end;

function TLibrariesDocumentsRoute.AsyncAwaitRetrieve(const LibraryId,
  DocumentId: string;
  const Callbacks: TFunc<TPromiseLibrariesDocuments>): TPromise<TLibrariesDocuments>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TLibrariesDocuments>(
    procedure(const CallbackParams: TFunc<TAsyncLibrariesDocuments>)
    begin
      AsyncRetrieve(LibraryId, DocumentId, CallbackParams);
    end,
    Callbacks);
end;

function TLibrariesDocumentsRoute.AsyncAwaitRetrieveStatus(const LibraryId,
  DocumentId: string;
  const Callbacks: TFunc<TPromiseLibraryDocumentsStatus>): TPromise<TLibraryDocumentsStatus>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TLibraryDocumentsStatus>(
    procedure(const CallbackParams: TFunc<TAsyncLibraryDocumentsStatus>)
    begin
      AsyncRetrieveStatus(LibraryId, DocumentId, CallbackParams);
    end,
    Callbacks);
end;

function TLibrariesDocumentsRoute.AsyncAwaitRetrieveText(const LibraryId,
  DocumentId: string;
  const Callbacks: TFunc<TPromiseLibraryDocumentsText>): TPromise<TLibraryDocumentsText>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TLibraryDocumentsText>(
    procedure(const CallbackParams: TFunc<TAsyncLibraryDocumentsText>)
    begin
      AsyncRetrieveText(LibraryId, DocumentId, CallbackParams);
    end,
    Callbacks);
end;

function TLibrariesDocumentsRoute.AsyncAwaitUpdate(const LibraryId,
  DocumentId: string;
  const ParamProc: TProc<TLibrariesDocumentsUpdateParams>;
  const Callbacks: TFunc<TPromiseLibrariesDocuments>): TPromise<TLibrariesDocuments>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TLibrariesDocuments>(
    procedure(const CallbackParams: TFunc<TAsyncLibrariesDocuments>)
    begin
      AsyncUpdate(LibraryId, DocumentId, ParamProc, CallbackParams);
    end,
    Callbacks);
end;

function TLibrariesDocumentsRoute.AsyncAwaitUpload(const LibraryId: string;
  const ParamProc: TProc<TLibrariesDocumentsUploadParams>;
  const Callbacks: TFunc<TPromiseLibrariesDocuments>): TPromise<TLibrariesDocuments>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TLibrariesDocuments>(
    procedure(const CallbackParams: TFunc<TAsyncLibrariesDocuments>)
    begin
      AsyncUpload(LibraryId, ParamProc, CallbackParams);
    end,
    Callbacks);
end;

procedure TLibrariesDocumentsRoute.AsyncDelete(const LibraryId,
  DocumentId: string; const Callbacks: TFunc<TAsyncLibraryDocumentsProcessed>);
begin
  with TAsyncCallBackExec<TAsyncLibraryDocumentsProcessed, TLibraryDocumentsProcessed>.Create(Callbacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TLibraryDocumentsProcessed
      begin
        Result := Self.Delete(LibraryId, DocumentId);
      end);
  finally
    Free;
  end;
end;

procedure TLibrariesDocumentsRoute.AsyncGetDocumentSignedUrl(const LibraryId,
  DocumentId: string; const Callbacks: TFunc<TAsyncSignedUrl>);
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
        Result := Self.GetDocumentSignedUrl(LibraryId, DocumentId);
      end);
  finally
    Free;
  end;
end;

procedure TLibrariesDocumentsRoute.AsyncGetTextSignedUrl(const LibraryId,
  DocumentId: string; const Callbacks: TFunc<TAsyncSignedUrl>);
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
        Result := Self.GetTextSignedUrl(LibraryId, DocumentId);
      end);
  finally
    Free;
  end;
end;

procedure TLibrariesDocumentsRoute.AsyncList(const LibraryId: string;
  const ParamProc: TProc<TLibrariesDocumentsUrlParams>;
  const Callbacks: TFunc<TAsyncLibrariesDocumentsList>);
begin
  with TAsyncCallBackExec<TAsyncLibrariesDocumentsList, TLibrariesDocumentsList>.Create(Callbacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TLibrariesDocumentsList
      begin
        Result := Self.List(LibraryId, ParamProc);
      end);
  finally
    Free;
  end;
end;

procedure TLibrariesDocumentsRoute.AsyncReprocess(const LibraryId,
  DocumentId: string; const Callbacks: TFunc<TAsyncLibraryDocumentsProcessed>);
begin
  with TAsyncCallBackExec<TAsyncLibraryDocumentsProcessed, TLibraryDocumentsProcessed>.Create(Callbacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TLibraryDocumentsProcessed
      begin
        Result := Self.Reprocess(LibraryId, DocumentId);
      end);
  finally
    Free;
  end;
end;

procedure TLibrariesDocumentsRoute.AsyncRetrieve(const LibraryId, DocumentId: string;
  const Callbacks: TFunc<TAsyncLibrariesDocuments>);
begin
  with TAsyncCallBackExec<TAsyncLibrariesDocuments, TLibrariesDocuments>.Create(Callbacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TLibrariesDocuments
      begin
        Result := Self.Retrieve(LibraryId, DocumentId);
      end);
  finally
    Free;
  end;
end;

procedure TLibrariesDocumentsRoute.AsyncRetrieveStatus(const LibraryId,
  DocumentId: string; const Callbacks: TFunc<TAsyncLibraryDocumentsStatus>);
begin
  with TAsyncCallBackExec<TAsyncLibraryDocumentsStatus, TLibraryDocumentsStatus>.Create(Callbacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TLibraryDocumentsStatus
      begin
        Result := Self.RetrieveStatus(LibraryId, DocumentId);
      end);
  finally
    Free;
  end;
end;

procedure TLibrariesDocumentsRoute.AsyncRetrieveText(const LibraryId,
  DocumentId: string; const Callbacks: TFunc<TAsyncLibraryDocumentsText>);
begin
  with TAsyncCallBackExec<TAsyncLibraryDocumentsText, TLibraryDocumentsText>.Create(Callbacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TLibraryDocumentsText
      begin
        Result := Self.RetrieveText(LibraryId, DocumentId);
      end);
  finally
    Free;
  end;
end;

procedure TLibrariesDocumentsRoute.AsyncUpdate(const LibraryId,
  DocumentId: string; const ParamProc: TProc<TLibrariesDocumentsUpdateParams>;
  const Callbacks: TFunc<TAsyncLibrariesDocuments>);
begin
  with TAsyncCallBackExec<TAsyncLibrariesDocuments, TLibrariesDocuments>.Create(Callbacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TLibrariesDocuments
      begin
        Result := Self.Update(LibraryId, DocumentId, ParamProc);
      end);
  finally
    Free;
  end;
end;

procedure TLibrariesDocumentsRoute.AsyncUpload(const LibraryId: string;
  const ParamProc: TProc<TLibrariesDocumentsUploadParams>;
  const Callbacks: TFunc<TAsyncLibrariesDocuments>);
begin
  with TAsyncCallBackExec<TAsyncLibrariesDocuments, TLibrariesDocuments>.Create(Callbacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TLibrariesDocuments
      begin
        Result := Self.Upload(LibraryId, ParamProc);
      end);
  finally
    Free;
  end;
end;

function TLibrariesDocumentsRoute.Delete(const LibraryId,
  DocumentId: string): TLibraryDocumentsProcessed;
begin
  Result := API.DeleteEx<TLibraryDocumentsProcessed>('libraries/' + LibraryId + '/documents/' + DocumentId);
end;

function TLibrariesDocumentsRoute.GetDocumentSignedUrl(const LibraryId,
  DocumentId: string): TSignedUrl;
begin
  Result := API.Get<TSignedUrl>('libraries/' + LibraryId + '/documents/' + DocumentId + '/signed-url', 'url');
end;

function TLibrariesDocumentsRoute.GetTextSignedUrl(const LibraryId,
  DocumentId: string): TSignedUrl;
begin
  Result := API.Get<TSignedUrl>('libraries/' + LibraryId + '/documents/' + DocumentId + '/extracted-text-signed-url', 'url');
end;

function TLibrariesDocumentsRoute.List(const LibraryId: string;
  const ParamProc: TProc<TLibrariesDocumentsUrlParams>): TLibrariesDocumentsList;
begin
  Result := API.Get<TLibrariesDocumentsList, TLibrariesDocumentsUrlParams>('libraries/' + LibraryId + '/documents', ParamProc);
end;

function TLibrariesDocumentsRoute.Reprocess(const LibraryId,
  DocumentId: string): TLibraryDocumentsProcessed;
begin
  Result := API.PostEx<TLibraryDocumentsProcessed>('libraries/' + LibraryId + '/documents/' + DocumentId + '/reprocess');
end;

function TLibrariesDocumentsRoute.Retrieve(const LibraryId,
  DocumentId: string): TLibrariesDocuments;
begin
  Result := API.Get<TLibrariesDocuments>('libraries/' + LibraryId + '/documents/' + DocumentId);
end;

function TLibrariesDocumentsRoute.RetrieveStatus(const LibraryId,
  DocumentId: string): TLibraryDocumentsStatus;
begin
  Result := API.Get<TLibraryDocumentsStatus>('libraries/' + LibraryId + '/documents/' + DocumentId + '/status');
end;

function TLibrariesDocumentsRoute.RetrieveText(const LibraryId,
  DocumentId: string): TLibraryDocumentsText;
begin
  Result := API.Get<TLibraryDocumentsText>('libraries/' + LibraryId + '/documents/' + DocumentId + '/text_content');
end;

function TLibrariesDocumentsRoute.Update(const LibraryId, DocumentId: string;
  const ParamProc: TProc<TLibrariesDocumentsUpdateParams>): TLibrariesDocuments;
begin
  Result := API.Put<TLibrariesDocuments, TLibrariesDocumentsUpdateParams>('libraries/' + LibraryId + '/documents/' + DocumentId, ParamProc);
end;

function TLibrariesDocumentsRoute.Upload(const LibraryId: string;
  const ParamProc: TProc<TLibrariesDocumentsUploadParams>): TLibrariesDocuments;
begin
  Result := API.PostForm<TLibrariesDocuments, TLibrariesDocumentsUploadParams>('libraries/' + LibraryId + '/documents', ParamProc);
end;

end.
