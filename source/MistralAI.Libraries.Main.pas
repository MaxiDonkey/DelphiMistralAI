unit MistralAI.Libraries.Main;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiMistralAI
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, System.Classes, System.Threading, System.JSON,
  REST.Json.Types, REST.JsonReflect,
  MistralAI.Types, MistralAI.API, MistralAI.API.Params,
  MistralAI.Async.Params, MistralAI.Async.Support, MistralAI.Async.Promise;

type
  /// <summary>
  /// Represents the parameters required to create or configure a document library
  /// within the Mistral AI API.
  /// </summary>
  /// <remarks>
  /// This class provides a fluent interface for defining the main properties of a library,
  /// such as its name, description, and chunk size for document indexing.
  /// Instances of this class are typically passed to methods like
  /// <c>TLibrariesMainRoute.Create</c> to create a new library.
  /// </remarks>
  TLibrariesMainParams = class(TJSONParam)
    /// <summary>
    /// Sets the name of the library.
    /// </summary>
    /// <param name="Value">
    /// The library name as a string.
    /// </param>
    /// <returns>
    /// The current instance of <c>TLibrariesMainParams</c> to allow method chaining.
    /// </returns>
    /// <remarks>
    /// The name should be unique within your organization and descriptive of the
    /// library's content.
    /// </remarks>
    function Name(const Value: string): TLibrariesMainParams;

    /// <summary>
    /// Sets the description of the library.
    /// </summary>
    /// <param name="Value">
    /// A string describing the purpose or content of the library.
    /// </param>
    /// <returns>
    /// The current instance of <c>TLibrariesMainParams</c> for fluent configuration.
    /// </returns>
    /// <remarks>
    /// The description can be updated later with the <c>Update</c> method on
    /// <c>TLibrariesMainRoute</c>.
    /// </remarks>
    function Description(const Value: string): TLibrariesMainParams;

    /// <summary>
    /// Sets the chunk size for the library.
    /// </summary>
    /// <param name="Value">
    /// The maximum chunk size (in characters or tokens, depending on backend settings)
    /// used to split documents during indexing.
    /// </param>
    /// <returns>
    /// The current instance of <c>TLibrariesMainParams</c> for method chaining.
    /// </returns>
    /// <remarks>
    /// Adjusting the chunk size can impact retrieval performance and accuracy when querying
    /// large documents. Larger chunks may improve context but reduce search granularity.
    /// </remarks>
    function ChunkSize(const Value: Integer): TLibrariesMainParams;
  end;

  /// <summary>
  /// Represents the parameters required to update the properties of an existing
  /// document library in the Mistral AI API.
  /// </summary>
  /// <remarks>
  /// This class provides a fluent interface for defining updated values such as
  /// the library name or description. It is primarily used with the
  /// <c>TLibrariesMainRoute.Update</c> method to modify an existing library's metadata.
  /// </remarks>
  TUpdateLibrariesMainParams = class(TJSONParam)
    /// <summary>
    /// Updates the name of the library.
    /// </summary>
    /// <param name="Value">
    /// The new name for the library as a string.
    /// </param>
    /// <returns>
    /// The current instance of <c>TUpdateLibrariesMainParams</c> for fluent configuration.
    /// </returns>
    /// <remarks>
    /// If not specified, the library's name remains unchanged.
    /// </remarks>
    function Name(const Value: string): TUpdateLibrariesMainParams;

    /// <summary>
    /// Updates the description of the library.
    /// </summary>
    /// <param name="Value">
    /// A new description providing details about the library's content or purpose.
    /// </param>
    /// <returns>
    /// The current instance of <c>TUpdateLibrariesMainParams</c> for method chaining.
    /// </returns>
    /// <remarks>
    /// If not specified, the library's description remains unchanged.
    /// </remarks>
    function Description(const Value: string): TUpdateLibrariesMainParams;
  end;

  /// <summary>
  /// Represents a document library in the Mistral AI API.
  /// </summary>
  /// <remarks>
  /// This class models the metadata of a document library, including its unique
  /// identifier, name, owner information, creation and update timestamps, as well as
  /// document statistics such as total size and number of documents.
  /// Instances of <c>TLibrariesMain</c> are typically returned by calls to
  /// <c>TLibrariesMainRoute.List</c>, <c>TLibrariesMainRoute.Create</c>,
  /// <c>TLibrariesMainRoute.Retrieve</c>, or <c>TLibrariesMainRoute.Update</c>.
  /// </remarks>
  TLibrariesMain = class(TJSONFingerprint)
  private
    FId: string;
    FName: string;
    [JsonNameAttribute('created_at')]
    FCreatedAt: string;
    [JsonNameAttribute('updated_at')]
    FUpdatedAt: string;
    [JsonNameAttribute('owner_id')]
    FOwnerId: string;
    [JsonNameAttribute('owner_type')]
    FOwnerType: string;
    [JsonNameAttribute('total_size')]
    FTotalSize: Int64;
    [JsonNameAttribute('nb_documents')]
    FNbDocuments: Int64;
    [JsonNameAttribute('chunk_size')]
    FChunkSize: Int64;
    FEmoji: string;
    FDescription: string;
    [JsonNameAttribute('generated_name')]
    FGeneratedName: string;
    [JsonNameAttribute('generated_description')]
    FGeneratedDescription: string;
    [JsonNameAttribute('explicit_user_members_count')]
    FExplicitUserMembersCount: Int64;
    [JsonNameAttribute('explicit_workspace_members_count')]
    FExplicitWorkspaceMembersCount: Int64;
    [JsonNameAttribute('org_sharing_role')]
    FOrgSharingRole: string;
  public
    /// <summary>
    /// Unique identifier of the library.
    /// </summary>
    property Id: string read FId write FId;

    /// <summary>
    /// Name of the library.
    /// </summary>
    property Name: string read FName write FName;

    /// <summary>
    /// ISO 8601 timestamp indicating when the library was created.
    /// </summary>
    property CreatedAt: string read FCreatedAt write FCreatedAt;

    /// <summary>
    /// ISO 8601 timestamp indicating when the library was last updated.
    /// </summary>
    property UpdatedAt: string read FUpdatedAt write FUpdatedAt;

    /// <summary>
    /// Identifier of the owner (user, workspace, or organization) that created the library.
    /// </summary>
    property OwnerId: string read FOwnerId write FOwnerId;

    /// <summary>
    /// Type of the owner (e.g., "User", "Workspace", or "Org").
    /// </summary>
    property OwnerType: string read FOwnerType write FOwnerType;

    /// <summary>
    /// Total size of all documents in the library, expressed in bytes.
    /// </summary>
    property TotalSize: Int64 read FTotalSize write FTotalSize;

    /// <summary>
    /// Total number of documents in the library.
    /// </summary>
    property NbDocuments: Int64 read FNbDocuments write FNbDocuments;

    /// <summary>
    /// Size of the chunks used for document indexing.
    /// </summary>
    property ChunkSize: Int64 read FChunkSize write FChunkSize;

    /// <summary>
    /// Emoji associated with the library (if any).
    /// </summary>
    property Emoji: string read FEmoji write FEmoji;

    /// <summary>
    /// User-provided description of the library.
    /// </summary>
    property Description: string read FDescription write FDescription;

    /// <summary>
    /// Automatically generated name for the library (if no explicit name was provided).
    /// </summary>
    property GeneratedName: string read FGeneratedName write FGeneratedName;

    /// <summary>
    /// Automatically generated description for the library (if no explicit description was provided).
    /// </summary>
    property GeneratedDescription: string read FGeneratedDescription write FGeneratedDescription;

    /// <summary>
    /// Number of users explicitly granted access to the library.
    /// </summary>
    property ExplicitUserMembersCount: Int64 read FExplicitUserMembersCount write FExplicitUserMembersCount;

    /// <summary>
    /// Number of workspaces explicitly granted access to the library.
    /// </summary>
    property ExplicitWorkspaceMembersCount: Int64 read FExplicitWorkspaceMembersCount write FExplicitWorkspaceMembersCount;

    /// <summary>
    /// Access level shared with the organization (e.g., "Viewer" or "Editor").
    /// </summary>
    property OrgSharingRole: string read FOrgSharingRole write FOrgSharingRole;
  end;

  /// <summary>
  /// Represents a list of document libraries retrieved from the Mistral AI API.
  /// </summary>
  /// <remarks>
  /// This class encapsulates an array of <c>TLibrariesMain</c> objects, each representing
  /// a single document library and its metadata (e.g., ID, name, description, and statistics).
  /// It is typically returned by calls such as <c>TLibrariesMainRoute.List</c> to enumerate
  /// all existing libraries.
  /// </remarks>
  TLibrariesMainList = class(TJSONFingerprint)
  private
    FData: TArray<TLibrariesMain>;
  public
    /// <summary>
    /// Array of <c>TLibrariesMain</c> objects representing the libraries.
    /// </summary>
    property Data: TArray<TLibrariesMain> read FData write FData;

    destructor Destroy; override;
  end;

  /// <summary>
  /// Represents an asynchronous callback handler for operations returning a
  /// <c>TLibrariesMain</c> instance.
  /// </summary>
  /// <remarks>
  /// This type alias specializes <c>TAsyncCallback</c> with <c>TLibrariesMain</c> as the
  /// result type. It is commonly used in non-blocking API calls such as
  /// <c>TLibrariesMainRoute.AsyncCreate</c>, <c>AsyncRetrieve</c>, or <c>AsyncUpdate</c>.
  /// <para>
  /// Handlers such as <c>OnStart</c>, <c>OnSuccess</c>, and <c>OnError</c> can be assigned
  /// to respond to the various stages of an asynchronous library operation.
  /// </para>
  /// </remarks>
  TAsyncLibrariesMain = TAsyncCallback<TLibrariesMain>;

  /// <summary>
  /// Represents a promise-based asynchronous handler for operations that return
  /// a <c>TLibrariesMain</c> instance.
  /// </summary>
  /// <remarks>
  /// This type alias specializes <c>TPromiseCallback</c> with <c>TLibrariesMain</c> as
  /// the result type. It is typically used with asynchronous workflows that follow a
  /// promise pattern, such as <c>TLibrariesMainRoute.AsyncAwaitCreate</c> or
  /// <c>AsyncAwaitUpdate</c>.
  /// <para>
  /// Using this type allows you to attach <c>OnSuccess</c> and <c>OnError</c> handlers
  /// in a chained style, simplifying non-blocking execution and result handling.
  /// </para>
  /// </remarks>
  TPromiseLibrariesMain = TPromiseCallback<TLibrariesMain>;

  /// <summary>
  /// Represents an asynchronous callback handler for operations that return
  /// a <c>TLibrariesMainList</c> instance.
  /// </summary>
  /// <remarks>
  /// This type alias specializes <c>TAsyncCallback</c> with <c>TLibrariesMainList</c>
  /// as the result type. It is typically used for non-blocking calls such as
  /// <c>TLibrariesMainRoute.AsyncList</c> to retrieve all libraries.
  /// <para>
  /// Assign event handlers like <c>OnStart</c>, <c>OnSuccess</c>, and <c>OnError</c>
  /// to respond to the different stages of the asynchronous operation.
  /// </para>
  /// </remarks>
  TAsyncLibrariesMainList = TAsyncCallback<TLibrariesMainList>;

  /// <summary>
  /// Represents a promise-based asynchronous handler for operations that return
  /// a <c>TLibrariesMainList</c> instance.
  /// </summary>
  /// <remarks>
  /// This type alias specializes <c>TPromiseCallback</c> with <c>TLibrariesMainList</c>
  /// as the result type. It is typically used with promise-oriented workflows,
  /// such as <c>TLibrariesMainRoute.AsyncAwaitList</c>, to retrieve all libraries.
  /// <para>
  /// This type allows attaching <c>OnSuccess</c> and <c>OnError</c> handlers in a
  /// chained style, making asynchronous code easier to manage compared to
  /// traditional callback patterns.
  /// </para>
  /// </remarks>
  TPromiseLibrariesMainList = TPromiseCallback<TLibrariesMainList>;

  TLibrariesMainRoute = class(TMistralAIAPIRoute)
    /// <summary>
    /// Asynchronously retrieves the list of document libraries and returns a promise
    /// that resolves with a <c>TLibrariesMainList</c> instance.
    /// </summary>
    /// <param name="Callbacks">
    /// An optional function that returns a <c>TPromiseLibrariesMainList</c> instance,
    /// allowing you to attach <c>OnSuccess</c> and <c>OnError</c> handlers in a
    /// promise-like style. If <c>nil</c>, no additional callbacks are registered.
    /// </param>
    /// <returns>
    /// A <c>TPromise&lt;TLibrariesMainList&gt;</c> that resolves with the list of
    /// libraries or rejects with an error if the operation fails.
    /// </returns>
    /// <remarks>
    /// Use this method when you want to perform a non-blocking call to retrieve all
    /// libraries, while benefiting from promise chaining and asynchronous error handling.
    /// </remarks>
    function AsyncAwaitList(
      const Callbacks: TFunc<TPromiseLibrariesMainList> = nil): TPromise<TLibrariesMainList>;

    /// <summary>
    /// Asynchronously creates a new document library and returns a promise
    /// that resolves with the resulting <c>TLibrariesMain</c> instance.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure used to configure the parameters of the new library
    /// through a <c>TLibrariesMainParams</c> instance (e.g., setting the
    /// name, description, and chunk size).
    /// </param>
    /// <param name="Callbacks">
    /// An optional function that returns a <c>TPromiseLibrariesMain</c> instance,
    /// allowing the assignment of <c>OnSuccess</c> and <c>OnError</c> handlers
    /// in a promise-like style. If <c>nil</c>, no additional callbacks are registered.
    /// </param>
    /// <returns>
    /// A <c>TPromise&lt;TLibrariesMain&gt;</c> that resolves with the newly
    /// created library or rejects with an error if the operation fails.
    /// </returns>
    /// <remarks>
    /// Use this method when you want to create a library in a non-blocking
    /// manner while benefiting from promise chaining for post-creation
    /// processing or error handling.
    /// </remarks>
    function AsyncAwaitCreate(
      const ParamProc: TProc<TLibrariesMainParams>;
      const Callbacks: TFunc<TPromiseLibrariesMain> = nil): TPromise<TLibrariesMain>;

    /// <summary>
    /// Asynchronously retrieves the details of a specific document library
    /// identified by its unique ID, and returns a promise that resolves
    /// with a <c>TLibrariesMain</c> instance.
    /// </summary>
    /// <param name="LibraryId">
    /// The unique identifier of the library to retrieve.
    /// </param>
    /// <param name="Callbacks">
    /// An optional function that returns a <c>TPromiseLibrariesMain</c> instance,
    /// allowing you to attach <c>OnSuccess</c> and <c>OnError</c> handlers
    /// in a promise-like style. If <c>nil</c>, no additional callbacks are registered.
    /// </param>
    /// <returns>
    /// A <c>TPromise&lt;TLibrariesMain&gt;</c> that resolves with the requested
    /// library details or rejects with an error if the operation fails.
    /// </returns>
    /// <remarks>
    /// Use this method for non-blocking retrieval of a library’s metadata,
    /// particularly when integrating with asynchronous workflows.
    /// </remarks>
    function AsyncAwaitRetrieve(const LibraryId: string;
      const Callbacks: TFunc<TPromiseLibrariesMain> = nil): TPromise<TLibrariesMain>;

    /// <summary>
    /// Asynchronously deletes a specific document library identified by its unique ID,
    /// and returns a promise that resolves with the deleted <c>TLibrariesMain</c> instance.
    /// </summary>
    /// <param name="LibraryId">
    /// The unique identifier of the library to delete.
    /// </param>
    /// <param name="Callbacks">
    /// An optional function that returns a <c>TPromiseLibrariesMain</c> instance,
    /// allowing you to attach <c>OnSuccess</c> and <c>OnError</c> handlers
    /// in a promise-like style. If <c>nil</c>, no additional callbacks are registered.
    /// </param>
    /// <returns>
    /// A <c>TPromise&lt;TLibrariesMain&gt;</c> that resolves with information
    /// about the deleted library or rejects with an error if the deletion fails.
    /// </returns>
    /// <remarks>
    /// Use this method to perform non-blocking deletions of libraries, typically in
    /// asynchronous workflows where promise chaining and error propagation are required.
    /// </remarks>
    function AsyncAwaitDelete(const LibraryId: string;
      const Callbacks: TFunc<TPromiseLibrariesMain> = nil): TPromise<TLibrariesMain>;

    /// <summary>
    /// Asynchronously updates the metadata of an existing document library identified
    /// by its unique ID, and returns a promise that resolves with the updated
    /// <c>TLibrariesMain</c> instance.
    /// </summary>
    /// <param name="LibraryId">
    /// The unique identifier of the library to update.
    /// </param>
    /// <param name="ParamProc">
    /// A procedure that configures the update parameters via a
    /// <c>TUpdateLibrariesMainParams</c> instance (e.g., updating the name or description).
    /// </param>
    /// <param name="Callbacks">
    /// An optional function that returns a <c>TPromiseLibrariesMain</c> instance,
    /// allowing you to attach <c>OnSuccess</c> and <c>OnError</c> handlers in a
    /// promise-like style. If <c>nil</c>, no additional callbacks are registered.
    /// </param>
    /// <returns>
    /// A <c>TPromise&lt;TLibrariesMain&gt;</c> that resolves with the updated library
    /// details or rejects with an error if the update operation fails.
    /// </returns>
    /// <remarks>
    /// Use this method for non-blocking updates of library metadata while benefiting
    /// from promise chaining and structured error handling.
    /// </remarks>
    function AsyncAwaitUpdate(const LibraryId: string;
      const ParamProc: TProc<TUpdateLibrariesMainParams>;
      const Callbacks: TFunc<TPromiseLibrariesMain> = nil): TPromise<TLibrariesMain>;

    /// <summary>
    /// Retrieves the list of all document libraries in a synchronous (blocking) manner.
    /// </summary>
    /// <returns>
    /// A <c>TLibrariesMainList</c> instance containing all libraries accessible
    /// within the current organization or context.
    /// </returns>
    /// <remarks>
    /// This method performs a blocking API call to fetch all libraries and their
    /// associated metadata. Use the asynchronous counterpart <c>AsyncList</c> or
    /// <c>AsyncAwaitList</c> for non-blocking operations.
    /// </remarks>
    function List: TLibrariesMainList;

    /// <summary>
    /// Creates a new document library in a synchronous (blocking) manner and returns
    /// the resulting <c>TLibrariesMain</c> instance.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure used to configure the parameters for the new library via a
    /// <c>TLibrariesMainParams</c> instance (e.g., setting name, description, or chunk size).
    /// </param>
    /// <returns>
    /// A <c>TLibrariesMain</c> instance representing the newly created library,
    /// including its unique ID and metadata.
    /// </returns>
    /// <remarks>
    /// This method performs a blocking API call. Use the asynchronous counterparts
    /// <c>AsyncCreate</c> or <c>AsyncAwaitCreate</c> if you need non-blocking behavior.
    /// An exception will be raised if the API call fails or if parameters are invalid.
    /// </remarks>
    function Create(const ParamProc: TProc<TLibrariesMainParams>): TLibrariesMain;

    /// <summary>
    /// Retrieves the details of a specific document library identified by its unique ID
    /// in a synchronous (blocking) manner.
    /// </summary>
    /// <param name="LibraryId">
    /// The unique identifier of the library to retrieve.
    /// </param>
    /// <returns>
    /// A <c>TLibrariesMain</c> instance containing the metadata of the requested library,
    /// such as its name, description, creation date, and document statistics.
    /// </returns>
    /// <remarks>
    /// This method performs a blocking API call. Use <c>AsyncRetrieve</c> or
    /// <c>AsyncAwaitRetrieve</c> for non-blocking operations.
    /// An exception will be raised if the specified library ID does not exist
    /// or if the request fails.
    /// </remarks>
    function Retrieve(const LibraryId: string): TLibrariesMain;

    /// <summary>
    /// Deletes a specific document library identified by its unique ID in a synchronous
    /// (blocking) manner.
    /// </summary>
    /// <param name="LibraryId">
    /// The unique identifier of the library to delete.
    /// </param>
    /// <returns>
    /// A <c>TLibrariesMain</c> instance representing the deleted library, including its
    /// metadata at the time of deletion.
    /// </returns>
    /// <remarks>
    /// This method performs a blocking API call to delete the library.
    /// Use <c>AsyncDelete</c> or <c>AsyncAwaitDelete</c> for non-blocking operations.
    /// An exception will be raised if the library ID is invalid, does not exist,
    /// or if the deletion fails due to permission issues.
    /// </remarks>
    function Delete(const LibraryId: string): TLibrariesMain;

    /// <summary>
    /// Updates the metadata of an existing document library identified by its unique ID
    /// in a synchronous (blocking) manner.
    /// </summary>
    /// <param name="LibraryId">
    /// The unique identifier of the library to update.
    /// </param>
    /// <param name="ParamProc">
    /// A procedure that configures the update parameters via a
    /// <c>TUpdateLibrariesMainParams</c> instance (e.g., modifying the library name or description).
    /// </param>
    /// <returns>
    /// A <c>TLibrariesMain</c> instance representing the updated library, including its
    /// new metadata after the update operation.
    /// </returns>
    /// <remarks>
    /// This method performs a blocking API call. Use <c>AsyncUpdate</c> or
    /// <c>AsyncAwaitUpdate</c> for non-blocking updates.
    /// An exception will be raised if the library ID is invalid, if no parameters are provided,
    /// or if the update operation fails due to permission issues.
    /// </remarks>
    function Update(const LibraryId: string;
      const ParamProc: TProc<TUpdateLibrariesMainParams>): TLibrariesMain;

    /// <summary>
    /// Asynchronously retrieves the list of all document libraries and triggers
    /// the specified callbacks at each stage of the operation.
    /// </summary>
    /// <param name="Callbacks">
    /// A function that returns a <c>TAsyncLibrariesMainList</c> instance,
    /// allowing you to assign <c>OnStart</c>, <c>OnSuccess</c>, and <c>OnError</c>
    /// event handlers to manage the lifecycle of the asynchronous call.
    /// </param>
    /// <remarks>
    /// This method performs a non-blocking retrieval of all libraries. Use
    /// <c>AsyncAwaitList</c> for a promise-based alternative that can be awaited.
    /// </remarks>
    procedure AsyncList(const Callbacks: TFunc<TAsyncLibrariesMainList>);

    /// <summary>
    /// Asynchronously creates a new document library and triggers the specified callbacks
    /// at each stage of the operation.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure used to configure the parameters for the new library through a
    /// <c>TLibrariesMainParams</c> instance (e.g., setting name, description, and chunk size).
    /// </param>
    /// <param name="Callbacks">
    /// A function that returns a <c>TAsyncLibrariesMain</c> instance, allowing you to
    /// assign <c>OnStart</c>, <c>OnSuccess</c>, and <c>OnError</c> event handlers
    /// to manage the lifecycle of the asynchronous call.
    /// </param>
    /// <remarks>
    /// This method performs a non-blocking library creation. Use <c>AsyncAwaitCreate</c>
    /// for a promise-based alternative.
    /// </remarks>
    procedure AsyncCreate(const ParamProc: TProc<TLibrariesMainParams>;
      const Callbacks: TFunc<TAsyncLibrariesMain>);

    /// <summary>
    /// Asynchronously retrieves the details of a specific document library
    /// identified by its unique ID and triggers the specified callbacks at
    /// each stage of the operation.
    /// </summary>
    /// <param name="LibraryId">
    /// The unique identifier of the library to retrieve.
    /// </param>
    /// <param name="Callbacks">
    /// A function that returns a <c>TAsyncLibrariesMain</c> instance,
    /// allowing you to assign <c>OnStart</c>, <c>OnSuccess</c>, and
    /// <c>OnError</c> event handlers to manage the asynchronous call.
    /// </param>
    /// <remarks>
    /// This method performs a non-blocking retrieval of a library's metadata.
    /// Use <c>AsyncAwaitRetrieve</c> if you prefer a promise-based approach.
    /// </remarks>
    procedure AsyncRetrieve(const LibraryId: string;
      const Callbacks: TFunc<TAsyncLibrariesMain>);

    /// <summary>
    /// Asynchronously deletes a specific document library identified by its unique ID,
    /// and triggers the specified callbacks at each stage of the operation.
    /// </summary>
    /// <param name="LibraryId">
    /// The unique identifier of the library to delete.
    /// </param>
    /// <param name="Callbacks">
    /// A function that returns a <c>TAsyncLibrariesMain</c> instance,
    /// allowing you to assign <c>OnStart</c>, <c>OnSuccess</c>, and
    /// <c>OnError</c> event handlers to manage the asynchronous call.
    /// </param>
    /// <remarks>
    /// This method performs a non-blocking deletion of a library.
    /// Use <c>AsyncA
    procedure AsyncDelete(const LibraryId: string;
      const Callbacks: TFunc<TAsyncLibrariesMain>);

    /// <summary>
    /// Asynchronously updates the metadata of an existing document library identified
    /// by its unique ID and triggers the specified callbacks at each stage of the operation.
    /// </summary>
    /// <param name="LibraryId">
    /// The unique identifier of the library to update.
    /// </param>
    /// <param name="ParamProc">
    /// A procedure used to configure the update parameters via a
    /// <c>TUpdateLibrariesMainParams</c> instance (e.g., updating the name or description).
    /// </param>
    /// <param name="Callbacks">
    /// A function that returns a <c>TAsyncLibrariesMain</c> instance,
    /// allowing you to assign <c>OnStart</c>, <c>OnSuccess</c>, and
    /// <c>OnError</c> event handlers to manage the asynchronous call.
    /// </param>
    /// <remarks>
    /// This method performs a non-blocking update of library metadata.
    /// Use <c>AsyncAwaitUpdate</c> for a promise-based alternative.
    /// </remarks>
    procedure AsyncUpdate(const LibraryId: string;
      const ParamProc: TProc<TUpdateLibrariesMainParams>;
      const Callbacks: TFunc<TAsyncLibrariesMain>);
  end;

implementation

{ TLibrariesMainList }

destructor TLibrariesMainList.Destroy;
begin
  for var Item in FData do
    Item.Free;
  inherited;
end;

{ TLibrariesMainRoute }

function TLibrariesMainRoute.AsyncAwaitCreate(
  const ParamProc: TProc<TLibrariesMainParams>;
  const Callbacks: TFunc<TPromiseLibrariesMain>): TPromise<TLibrariesMain>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TLibrariesMain>(
    procedure(const CallbackParams: TFunc<TAsyncLibrariesMain>)
    begin
      AsyncCreate(ParamProc, CallbackParams);
    end,
    Callbacks);
end;

function TLibrariesMainRoute.AsyncAwaitDelete(const LibraryId: string;
  const Callbacks: TFunc<TPromiseLibrariesMain>): TPromise<TLibrariesMain>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TLibrariesMain>(
    procedure(const CallbackParams: TFunc<TAsyncLibrariesMain>)
    begin
      AsyncDelete(LibraryId, CallbackParams);
    end,
    Callbacks);
end;

function TLibrariesMainRoute.AsyncAwaitList(
  const Callbacks: TFunc<TPromiseLibrariesMainList>): TPromise<TLibrariesMainList>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TLibrariesMainList>(
    procedure(const CallbackParams: TFunc<TAsyncLibrariesMainList>)
    begin
      AsyncList(CallbackParams);
    end,
    Callbacks);
end;

function TLibrariesMainRoute.AsyncAwaitRetrieve(const LibraryId: string;
  const Callbacks: TFunc<TPromiseLibrariesMain>): TPromise<TLibrariesMain>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TLibrariesMain>(
    procedure(const CallbackParams: TFunc<TAsyncLibrariesMain>)
    begin
      AsyncRetrieve(LibraryId, CallbackParams);
    end,
    Callbacks);
end;

function TLibrariesMainRoute.AsyncAwaitUpdate(const LibraryId: string;
  const ParamProc: TProc<TUpdateLibrariesMainParams>;
  const Callbacks: TFunc<TPromiseLibrariesMain>): TPromise<TLibrariesMain>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TLibrariesMain>(
    procedure(const CallbackParams: TFunc<TAsyncLibrariesMain>)
    begin
      AsyncUpdate(LibraryId, ParamProc, CallbackParams);
    end,
    Callbacks);
end;

procedure TLibrariesMainRoute.AsyncCreate(
  const ParamProc: TProc<TLibrariesMainParams>;
  const Callbacks: TFunc<TAsyncLibrariesMain>);
begin
  with TAsyncCallBackExec<TAsyncLibrariesMain, TLibrariesMain>.Create(Callbacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TLibrariesMain
      begin
        Result := Self.Create(ParamProc);
      end);
  finally
    Free;
  end;
end;

procedure TLibrariesMainRoute.AsyncDelete(const LibraryId: string;
  const Callbacks: TFunc<TAsyncLibrariesMain>);
begin
  with TAsyncCallBackExec<TAsyncLibrariesMain, TLibrariesMain>.Create(Callbacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TLibrariesMain
      begin
        Result := Self.Delete(LibraryId);
      end);
  finally
    Free;
  end;
end;

procedure TLibrariesMainRoute.AsyncList(
  const Callbacks: TFunc<TAsyncLibrariesMainList>);
begin
  with TAsyncCallBackExec<TAsyncLibrariesMainList, TLibrariesMainList>.Create(Callbacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TLibrariesMainList
      begin
        Result := Self.List;
      end);
  finally
    Free;
  end;
end;

procedure TLibrariesMainRoute.AsyncRetrieve(const LibraryId: string;
  const Callbacks: TFunc<TAsyncLibrariesMain>);
begin
  with TAsyncCallBackExec<TAsyncLibrariesMain, TLibrariesMain>.Create(Callbacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TLibrariesMain
      begin
        Result := Self.Retrieve(LibraryId);
      end);
  finally
    Free;
  end;
end;

procedure TLibrariesMainRoute.AsyncUpdate(const LibraryId: string;
  const ParamProc: TProc<TUpdateLibrariesMainParams>;
  const Callbacks: TFunc<TAsyncLibrariesMain>);
begin
  with TAsyncCallBackExec<TAsyncLibrariesMain, TLibrariesMain>.Create(Callbacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TLibrariesMain
      begin
        Result := Self.Update(LibraryId, ParamProc);
      end);
  finally
    Free;
  end;
end;

function TLibrariesMainRoute.Create(
  const ParamProc: TProc<TLibrariesMainParams>): TLibrariesMain;
begin
  Result := API.Post<TLibrariesMain, TLibrariesMainParams>('libraries', ParamProc);
end;

function TLibrariesMainRoute.Delete(const LibraryId: string): TLibrariesMain;
begin
  Result := API.Delete<TLibrariesMain>('libraries/' + LibraryId);
end;

function TLibrariesMainRoute.List: TLibrariesMainList;
begin
  Result := API.Get<TLibrariesMainList>('libraries');
end;

function TLibrariesMainRoute.Retrieve(const LibraryId: string): TLibrariesMain;
begin
  Result := API.Get<TLibrariesMain>('libraries/' + LibraryId);
end;

function TLibrariesMainRoute.Update(const LibraryId: string;
  const ParamProc: TProc<TUpdateLibrariesMainParams>): TLibrariesMain;
begin
  Result := API.Put<TLibrariesMain, TUpdateLibrariesMainParams>('libraries/' + LibraryId, ParamProc);
end;

{ TLibrariesMainParams }

function TLibrariesMainParams.ChunkSize(
  const Value: Integer): TLibrariesMainParams;
begin
  Result := TLibrariesMainParams(Add('chunk_size', Value));
end;

function TLibrariesMainParams.Description(
  const Value: string): TLibrariesMainParams;
begin
  Result := TLibrariesMainParams(Add('description', Value));
end;

function TLibrariesMainParams.Name(const Value: string): TLibrariesMainParams;
begin
  Result := TLibrariesMainParams(Add('name', Value));
end;

{ TUpdateLibrariesMainParams }

function TUpdateLibrariesMainParams.Description(
  const Value: string): TUpdateLibrariesMainParams;
begin
  Result := TUpdateLibrariesMainParams(Add('description', Value));
end;

function TUpdateLibrariesMainParams.Name(
  const Value: string): TUpdateLibrariesMainParams;
begin
  Result := TUpdateLibrariesMainParams(Add('name', Value));
end;

end.
