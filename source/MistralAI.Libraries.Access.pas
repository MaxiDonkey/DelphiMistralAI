unit MistralAI.Libraries.Access;

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
  /// Represents the parameters required to create or update access levels for a library.
  /// </summary>
  /// <remarks>
  /// The <c>TAccessParams</c> class is used to build the request payload when assigning or modifying
  /// access levels to a specific entity (user, workspace, or organization) for a library.
  /// It provides fluent methods for configuring organization ID, access level, and the target entity.
  /// </remarks>
  TAccessParams = class(TJSONParam)
    /// <summary>
    /// Sets the organization ID associated with the library.
    /// </summary>
    /// <param name="Value">
    /// The unique identifier (<c>UUID</c>) of the organization.
    /// </param>
    /// <returns>
    /// The current <c>TAccessParams</c> instance for fluent chaining.
    /// </returns>
    function OrgId(const Value: string): TAccessParams;

    /// <summary>
    /// Sets the access level for the entity using a <c>TLevelType</c> value.
    /// </summary>
    /// <param name="Value">
    /// A <c>TLevelType</c> enumeration value indicating the level of access (e.g., Viewer or Editor).
    /// </param>
    /// <returns>
    /// The current <c>TAccessParams</c> instance for fluent chaining.
    /// </returns>
    function Level(const Value: TLevelType): TAccessParams; overload;

    /// <summary>
    /// Sets the access level for the entity using a string value.
    /// </summary>
    /// <param name="Value">
    /// A string representing the level of access (e.g., "Viewer" or "Editor").
    /// </param>
    /// <returns>
    /// The current <c>TAccessParams</c> instance for fluent chaining.
    /// </returns>
    function Level(const Value: string): TAccessParams; overload;

    /// <summary>
    /// Sets the unique identifier (<c>UUID</c>) of the entity to share the library with.
    /// </summary>
    /// <param name="Value">
    /// The entity's unique ID (<c>UUID</c>), such as a user ID, workspace ID, or organization ID.
    /// </param>
    /// <returns>
    /// The current <c>TAccessParams</c> instance for fluent chaining.
    /// </returns>
    function ShareWithUuid(const Value: string): TAccessParams;

    /// <summary>
    /// Sets the type of the entity to share the library with using a <c>TShareWithType</c> value.
    /// </summary>
    /// <param name="Value">
    /// A <c>TShareWithType</c> enumeration value indicating the target entity type (User, Workspace, or Org).
    /// </param>
    /// <returns>
    /// The current <c>TAccessParams</c> instance for fluent chaining.
    /// </returns>
    function ShareWithType(const Value: TShareWithType): TAccessParams; overload;

    /// <summary>
    /// Sets the type of the entity to share the library with using a string value.
    /// </summary>
    /// <param name="Value">
    /// A string representing the entity type (e.g., "User", "Workspace", or "Org").
    /// </param>
    /// <returns>
    /// The current <c>TAccessParams</c> instance for fluent chaining.
    /// </returns>
    function ShareWithType(const Value: string): TAccessParams; overload;
  end;

  /// <summary>
  /// Represents the parameters required to delete access levels for a library.
  /// </summary>
  /// <remarks>
  /// The <c>TAccessDeleteParams</c> class is used to build the request payload when
  /// revoking access rights for a specific entity (user, workspace, or organization)
  /// associated with a library.
  /// </remarks>
  TAccessDeleteParams = class(TJSONParam)
    /// <summary>
    /// Sets the organization ID associated with the library.
    /// </summary>
    /// <param name="Value">
    /// The unique identifier (<c>UUID</c>) of the organization.
    /// </param>
    /// <returns>
    /// The current <c>TAccessDeleteParams</c> instance for fluent chaining.
    /// </returns>
    function OrgId(const Value: string): TAccessDeleteParams;

    /// <summary>
    /// Sets the unique identifier (<c>UUID</c>) of the entity whose access will be revoked.
    /// </summary>
    /// <param name="Value">
    /// The entity's unique ID (<c>UUID</c>), such as a user ID, workspace ID, or organization ID.
    /// </param>
    /// <returns>
    /// The current <c>TAccessDeleteParams</c> instance for fluent chaining.
    /// </returns>
    function ShareWithUuid(const Value: string): TAccessDeleteParams;

    /// <summary>
    /// Sets the type of the entity whose access will be revoked using a <c>TShareWithType</c> value.
    /// </summary>
    /// <param name="Value">
    /// A <c>TShareWithType</c> enumeration value indicating the target entity type (User, Workspace, or Org).
    /// </param>
    /// <returns>
    /// The current <c>TAccessDeleteParams</c> instance for fluent chaining.
    /// </returns>
    function ShareWithType(const Value: TShareWithType): TAccessDeleteParams; overload;

    /// <summary>
    /// Sets the type of the entity whose access will be revoked using a string value.
    /// </summary>
    /// <param name="Value">
    /// A string representing the entity type (e.g., "User", "Workspace", or "Org").
    /// </param>
    /// <returns>
    /// The current <c>TAccessDeleteParams</c> instance for fluent chaining.
    /// </returns>
    function ShareWithType(const Value: string): TAccessDeleteParams; overload;
  end;

  /// <summary>
  /// Represents the access rights of an entity (user, workspace, or organization) to a specific library.
  /// </summary>
  /// <remarks>
  /// The <c>TLibrariesAccess</c> class models the response object for library access queries or updates.
  /// It contains information about the library, the organization, the entity with which it is shared,
  /// and the access role granted.
  /// </remarks>
  TLibrariesAccess = class(TJSONFingerprint)
  private
    [JsonNameAttribute('library_id')]
    FLibraryId: string;
    [JsonNameAttribute('user_id')]
    FUserId: string;
    [JsonNameAttribute('org_id')]
    FOrgId: string;
    FRole: string;
    [JsonNameAttribute('share_with_type')]
    [JsonReflectAttribute(ctString, rtString, TShareWithTypeInterceptor)]
    FShareWithType: TShareWithType;
    [JsonNameAttribute('share_with_uuid')]
    FShareWithUuid: string;
  public
    /// <summary>
    /// Gets or sets the unique identifier (<c>UUID</c>) of the library.
    /// </summary>
    /// <value>
    /// A string representing the library ID.
    /// </value>
    property LibraryId: string read FLibraryId write FLibraryId;

    /// <summary>
    /// Gets or sets the user ID associated with the access, if applicable.
    /// </summary>
    /// <value>
    /// A string representing the user ID, or <c>nil</c> if the access is not user-specific.
    /// </value>
    property UserId: string read FUserId write FUserId;

    /// <summary>
    /// Gets or sets the organization ID associated with the access.
    /// </summary>
    /// <value>
    /// A string representing the organization ID (<c>UUID</c>).
    /// </value>
    property OrgId: string read FOrgId write FOrgId;

    /// <summary>
    /// Gets or sets the role assigned to the entity.
    /// </summary>
    /// <value>
    /// A string describing the access role (e.g., "Viewer" or "Editor").
    /// </value>
    property Role: string read FRole write FRole;

    /// <summary>
    /// Gets or sets the type of the entity that has access to the library.
    /// </summary>
    /// <value>
    /// A <c>TShareWithType</c> enumeration value indicating the entity type (User, Workspace, or Org).
    /// </value>
    property ShareWithType: TShareWithType read FShareWithType write FShareWithType;

    /// <summary>
    /// Gets or sets the unique identifier (<c>UUID</c>) of the entity with access.
    /// </summary>
    /// <value>
    /// A string representing the entity ID (<c>UUID</c>).
    /// </value>
    property ShareWithUuid: string read FShareWithUuid write FShareWithUuid;
  end;

  /// <summary>
  /// Represents a list of access rights for a specific library.
  /// </summary>
  /// <remarks>
  /// The <c>TLibrariesAccessList</c> class models the response when querying all entities that have access
  /// to a library. It encapsulates an array of <c>TLibrariesAccess</c> objects, each describing the
  /// access details for a single entity (user, workspace, or organization).
  /// </remarks>
  TLibrariesAccessList = class(TJSONFingerprint)
  private
    FData: TArray<TLibrariesAccess>;
  public
    /// <summary>
    /// Gets or sets the list of access entries for the library.
    /// </summary>
    /// <value>
    /// An array of <c>TLibrariesAccess</c> objects, where each object represents
    /// the access information of an entity.
    /// </value>
    property Data: TArray<TLibrariesAccess> read FData write FData;

    /// <summary>
    /// Destroys the current instance of <c>TLibrariesAccessList</c> and releases
    /// any resources associated with the contained <c>TLibrariesAccess</c> objects.
    /// </summary>
    destructor Destroy; override;
  end;

  /// <summary>
  /// Represents an asynchronous callback handler for library access operations.
  /// </summary>
  /// <remarks>
  /// <c>TAsyncLibrariesAccess</c> is a type alias for <c>TAsyncCallback&lt;TLibrariesAccess&gt;</c>.
  /// It is used to manage non-blocking operations related to a single library access entry,
  /// providing event hooks such as <c>OnStart</c>, <c>OnSuccess</c>, and <c>OnError</c>.
  /// </remarks>
  TAsyncLibrariesAccess = TAsyncCallback<TLibrariesAccess>;

  /// <summary>
  /// Represents a promise-based asynchronous handler for library access operations.
  /// </summary>
  /// <remarks>
  /// <c>TPromiseLibrariesAccess</c> is a type alias for <c>TPromiseCallback&lt;TLibrariesAccess&gt;</c>.
  /// It is used in asynchronous workflows to retrieve or update a single library access entry,
  /// resolving with a <c>TLibrariesAccess</c> result when the operation completes successfully,
  /// or rejecting with an exception if an error occurs.
  /// </remarks>
  TPromiseLibrariesAccess = TPromiseCallback<TLibrariesAccess>;

  /// <summary>
  /// Represents an asynchronous callback handler for operations returning a list of library access entries.
  /// </summary>
  /// <remarks>
  /// <c>TAsyncLibrariesAccessList</c> is a type alias for <c>TAsyncCallback&lt;TLibrariesAccessList&gt;</c>.
  /// It is used for non-blocking operations that retrieve all access entries of a specific library,
  /// providing event hooks such as <c>OnStart</c>, <c>OnSuccess</c>, and <c>OnError</c> to manage the asynchronous workflow.
  /// </remarks>
  TAsyncLibrariesAccessList = TAsyncCallback<TLibrariesAccessList>;

  /// <summary>
  /// Represents a promise-based asynchronous handler for operations returning a list of library access entries.
  /// </summary>
  /// <remarks>
  /// <c>TPromiseLibrariesAccessList</c> is a type alias for <c>TPromiseCallback&lt;TLibrariesAccessList&gt;</c>.
  /// It is used in asynchronous workflows to retrieve all access entries of a specific library,
  /// resolving with a <c>TLibrariesAccessList</c> when the operation completes successfully,
  /// or rejecting with an exception if an error occurs.
  /// </remarks>
  TPromiseLibrariesAccessList = TPromiseCallback<TLibrariesAccessList>;


  /// <summary>
  /// Provides methods to manage and query access permissions for libraries.
  /// </summary>
  /// <remarks>
  /// The <c>TLibrariesAccessRoute</c> class acts as a client-side abstraction for the
  /// <c>/v1/libraries/{library_id}/share</c> API endpoints.
  /// It allows retrieving, creating, updating, and deleting access levels for users,
  /// workspaces, or organizations associated with a library.
  /// <para>
  /// The class supports both synchronous operations (e.g., <c>List</c>, <c>CreateOrUpdate</c>, <c>Delete</c>)
  /// and asynchronous operations using callbacks (e.g., <c>AsyncList</c>) or promises
  /// (e.g., <c>AsyncAwaitList</c>).
  /// </para>
  /// <para>
  /// To modify access rights, the caller must have ownership permissions for the specified library.
  /// An owner cannot remove their own access.
  /// </para>
  /// </remarks>
  TLibrariesAccessRoute = class(TMistralAIAPIRoute)
    /// <summary>
    /// Asynchronously retrieves the list of all entities that have access to the specified library,
    /// returning a promise that resolves with the result.
    /// </summary>
    /// <param name="LibraryId">
    /// The unique identifier (<c>UUID</c>) of the library whose access permissions are being queried.
    /// </param>
    /// <param name="Callbacks">
    /// An optional function returning a <c>TPromiseLibrariesAccessList</c>, allowing configuration of
    /// promise-style handlers such as <c>OnSuccess</c> and <c>OnError</c>.
    /// If not provided, default behavior is applied.
    /// </param>
    /// <returns>
    /// A <c>TPromise&lt;TLibrariesAccessList&gt;</c> that resolves with a <c>TLibrariesAccessList</c>
    /// when the operation completes successfully or rejects with an exception if an error occurs.
    /// </returns>
    /// <remarks>
    /// This method performs a non-blocking <c>GET</c> request to the API endpoint
    /// <c>/v1/libraries/{library_id}/share</c>.
    /// Use this function in asynchronous workflows where <c>await</c> or promise chaining is preferred.
    /// </remarks>
    function AsyncAwaitList(const LibraryId: string;
      const Callbacks: TFunc<TPromiseLibrariesAccessList> = nil): TPromise<TLibrariesAccessList>;

    /// <summary>
    /// Asynchronously creates or updates the access level of an entity for a specified library,
    /// returning a promise that resolves with the result.
    /// </summary>
    /// <param name="LibraryId">
    /// The unique identifier (<c>UUID</c>) of the library for which the access level will be created or updated.
    /// </param>
    /// <param name="ParamProc">
    /// A procedure that configures the <c>TAccessParams</c> object, specifying details such as
    /// organization ID, access level (<c>Viewer</c> or <c>Editor</c>), and the target entity
    /// (user, workspace, or organization) to share the library with.
    /// </param>
    /// <param name="Callbacks">
    /// An optional function returning a <c>TPromiseLibrariesAccess</c>, allowing configuration of
    /// promise-style handlers such as <c>OnSuccess</c> and <c>OnError</c>.
    /// If not provided, default promise behavior is used.
    /// </param>
    /// <returns>
    /// A <c>TPromise&lt;TLibrariesAccess&gt;</c> that resolves with a <c>TLibrariesAccess</c> instance
    /// containing the updated access information, or rejects with an exception if an error occurs.
    /// </returns>
    /// <remarks>
    /// This method performs a non-blocking <c>PUT</c> request to the API endpoint
    /// <c>/v1/libraries/{library_id}/share</c>.
    /// It is ideal for asynchronous workflows using <c>await</c> or promise chaining.
    /// </remarks>
    function AsyncAwaitCreateOrUpdate(const LibraryId: string;
      const ParamProc: TProc<TAccessParams>;
      const Callbacks: TFunc<TPromiseLibrariesAccess> = nil): TPromise<TLibrariesAccess>;

    /// <summary>
    /// Asynchronously deletes the access rights of an entity for a specified library,
    /// returning a promise that resolves with the result.
    /// </summary>
    /// <param name="LibraryId">
    /// The unique identifier (<c>UUID</c>) of the library from which the access rights will be revoked.
    /// </param>
    /// <param name="ParamProc">
    /// A procedure that configures the <c>TAccessParams</c> object, specifying details such as
    /// the organization ID and the target entity (user, workspace, or organization) whose access should be removed.
    /// </param>
    /// <param name="Callbacks">
    /// An optional function returning a <c>TPromiseLibrariesAccess</c>, allowing configuration of
    /// promise-style handlers such as <c>OnSuccess</c> and <c>OnError</c>.
    /// If not provided, default promise behavior is applied.
    /// </param>
    /// <returns>
    /// A <c>TPromise&lt;TLibrariesAccess&gt;</c> that resolves with a <c>TLibrariesAccess</c> instance
    /// describing the deleted access entry, or rejects with an exception if an error occurs.
    /// </returns>
    /// <remarks>
    /// This method performs a non-blocking <c>DELETE</c> request to the API endpoint
    /// <c>/v1/libraries/{library_id}/share</c>.
    /// It is designed for asynchronous workflows using <c>await</c> or promise chaining.
    /// </remarks>
    function AsyncAwaitDelete(const LibraryId: string;
      const ParamProc: TProc<TAccessParams>;
      const Callbacks: TFunc<TPromiseLibrariesAccess> = nil): TPromise<TLibrariesAccess>;

    /// <summary>
    /// Retrieves the list of all entities that have access to the specified library.
    /// </summary>
    /// <param name="LibraryId">
    /// The unique identifier (<c>UUID</c>) of the library whose access permissions are being queried.
    /// </param>
    /// <returns>
    /// A <c>TLibrariesAccessList</c> instance containing all access entries for the specified library,
    /// including entity type, UUID, organization ID, and assigned role.
    /// </returns>
    /// <remarks>
    /// This method performs a synchronous call to the API endpoint
    /// <c>GET /v1/libraries/{library_id}/share</c> and blocks until the response is received.
    /// Use this method when immediate access to the complete list of entities is required.
    /// </remarks>
    function List(const LibraryId: string): TLibrariesAccessList;

    /// <summary>
    /// Creates or updates the access level of an entity for a specified library.
    /// </summary>
    /// <param name="LibraryId">
    /// The unique identifier (<c>UUID</c>) of the library for which the access level will be created or updated.
    /// </param>
    /// <param name="ParamProc">
    /// A procedure that configures the <c>TAccessParams</c> object, specifying details such as
    /// organization ID, access level (<c>Viewer</c> or <c>Editor</c>), and the entity (user, workspace, or organization)
    /// to share the library with.
    /// </param>
    /// <returns>
    /// A <c>TLibrariesAccess</c> instance containing the updated access information for the specified entity.
    /// </returns>
    /// <remarks>
    /// This method sends a synchronous <c>PUT</c> request to the API endpoint
    /// <c>/v1/libraries/{library_id}/share</c>.
    /// The caller must have ownership rights for the library in order to create or modify access levels.
    /// </remarks>
    function CreateOrUpdate(const LibraryId: string;
      const ParamProc: TProc<TAccessParams>): TLibrariesAccess;

    /// <summary>
    /// Deletes the access rights of an entity for a specified library.
    /// </summary>
    /// <param name="LibraryId">
    /// The unique identifier (<c>UUID</c>) of the library for which the access rights will be removed.
    /// </param>
    /// <param name="ParamProc">
    /// A procedure that configures the <c>TAccessParams</c> object, specifying details such as
    /// organization ID and the entity (user, workspace, or organization) whose access must be revoked.
    /// </param>
    /// <returns>
    /// A <c>TLibrariesAccess</c> instance containing the access information that was removed.
    /// </returns>
    /// <remarks>
    /// This method sends a synchronous <c>DELETE</c> request to the API endpoint
    /// <c>/v1/libraries/{library_id}/share</c>.
    /// The caller must be the owner of the library to revoke access from other entities.
    /// An owner cannot delete their own access.
    /// </remarks>
    function Delete(const LibraryId: string;
      const ParamProc: TProc<TAccessParams>): TLibrariesAccess;

    /// <summary>
    /// Asynchronously retrieves the list of all entities that have access to the specified library.
    /// </summary>
    /// <param name="LibraryId">
    /// The unique identifier (<c>UUID</c>) of the library whose access permissions are being queried.
    /// </param>
    /// <param name="Callbacks">
    /// A function returning a <c>TAsyncLibrariesAccessList</c> record, which allows configuration
    /// of asynchronous event handlers such as <c>OnStart</c>, <c>OnSuccess</c>, and <c>OnError</c>.
    /// </param>
    /// <remarks>
    /// This method triggers a non-blocking operation to call the API endpoint
    /// <c>GET /v1/libraries/{library_id}/share</c>.
    /// The result, encapsulated in a <c>TLibrariesAccessList</c>, is delivered through the
    /// <c>OnSuccess</c> callback when the request completes successfully, or <c>OnError</c> if it fails.
    /// </remarks>
    procedure AsyncList(const LibraryId: string;
      const Callbacks: TFunc<TAsyncLibrariesAccessList>);

    /// <summary>
    /// Asynchronously creates or updates the access level of an entity for a specified library.
    /// </summary>
    /// <param name="LibraryId">
    /// The unique identifier (<c>UUID</c>) of the library for which the access level will be created or updated.
    /// </param>
    /// <param name="ParamProc">
    /// A procedure that configures the <c>TAccessParams</c> object, specifying details such as
    /// organization ID, access level (<c>Viewer</c> or <c>Editor</c>), and the target entity
    /// (user, workspace, or organization) with which to share the library.
    /// </param>
    /// <param name="Callbacks">
    /// A function returning a <c>TAsyncLibrariesAccess</c> record, which allows configuration
    /// of asynchronous event handlers such as <c>OnStart</c>, <c>OnSuccess</c>, and <c>OnError</c>.
    /// </param>
    /// <remarks>
    /// This method performs a non-blocking <c>PUT</c> request to the API endpoint
    /// <c>/v1/libraries/{library_id}/share</c>.
    /// The resulting <c>TLibrariesAccess</c> instance is provided through the <c>OnSuccess</c> callback
    /// when the operation completes, or <c>OnError</c> if an error occurs.
    /// </remarks>
    procedure AsyncCreateOrUpdate(const LibraryId: string;
      const ParamProc: TProc<TAccessParams>;
      const Callbacks: TFunc<TAsyncLibrariesAccess>);

    /// <summary>
    /// Asynchronously deletes the access rights of an entity for a specified library.
    /// </summary>
    /// <param name="LibraryId">
    /// The unique identifier (<c>UUID</c>) of the library for which the access rights will be revoked.
    /// </param>
    /// <param name="ParamProc">
    /// A procedure that configures the <c>TAccessParams</c> object, specifying details such as
    /// organization ID and the entity (user, workspace, or organization) whose access will be removed.
    /// </param>
    /// <param name="Callbacks">
    /// A function returning a <c>TAsyncLibrariesAccess</c> record, allowing configuration
    /// of asynchronous event handlers such as <c>OnStart</c>, <c>OnSuccess</c>, and <c>OnError</c>.
    /// </param>
    /// <remarks>
    /// This method performs a non-blocking <c>DELETE</c> request to the API endpoint
    /// <c>/v1/libraries/{library_id}/share</c>.
    /// The result, a <c>TLibrariesAccess</c> instance describing the deleted access,
    /// is delivered through the <c>OnSuccess</c> callback, or <c>OnError</c> if the operation fails.
    /// </remarks>
    procedure AsyncDelete(const LibraryId: string;
      const ParamProc: TProc<TAccessParams>;
      const Callbacks: TFunc<TAsyncLibrariesAccess>);
  end;

implementation

{ TAccessParams }

function TAccessParams.Level(
  const Value: TLevelType): TAccessParams;
begin
  Result := TAccessParams(Add('level', Value.ToString));
end;

function TAccessParams.Level(
  const Value: string): TAccessParams;
begin
  Result := TAccessParams(Add('level',  TLevelType.Create(Value).ToString));
end;

function TAccessParams.OrgId(
  const Value: string): TAccessParams;
begin
  Result := TAccessParams(Add('org_id', Value));
end;

function TAccessParams.ShareWithType(
  const Value: TShareWithType): TAccessParams;
begin
  Result := TAccessParams(Add('share_with_type', Value.ToString));
end;

function TAccessParams.ShareWithType(
  const Value: string): TAccessParams;
begin
  Result := TAccessParams(Add('share_with_type', TShareWithType.Create(Value).ToString));
end;

function TAccessParams.ShareWithUuid(
  const Value: string): TAccessParams;
begin
  Result := TAccessParams(Add('share_with_uuid', Value));
end;

{ TAccessDeleteParams }

function TAccessDeleteParams.OrgId(const Value: string): TAccessDeleteParams;
begin
  Result := TAccessDeleteParams(Add('org_id', Value));
end;

function TAccessDeleteParams.ShareWithType(
  const Value: TShareWithType): TAccessDeleteParams;
begin
  Result := TAccessDeleteParams(Add('share_with_type', Value.ToString));
end;

function TAccessDeleteParams.ShareWithType(
  const Value: string): TAccessDeleteParams;
begin
  Result := TAccessDeleteParams(Add('share_with_type', TShareWithType.Create(Value).ToString));
end;

function TAccessDeleteParams.ShareWithUuid(const Value: string): TAccessDeleteParams;
begin
  Result := TAccessDeleteParams(Add('share_with_uuid', Value));
end;

{ TLibrariesAccessList }

destructor TLibrariesAccessList.Destroy;
begin
  for var Item in FData do
    Item.Free;
  inherited;
end;

{ TLibrariesAccessRoute }

function TLibrariesAccessRoute.AsyncAwaitCreateOrUpdate(const LibraryId: string;
  const ParamProc: TProc<TAccessParams>;
  const Callbacks: TFunc<TPromiseLibrariesAccess>): TPromise<TLibrariesAccess>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TLibrariesAccess>(
    procedure(const CallbackParams: TFunc<TAsyncLibrariesAccess>)
    begin
      AsyncCreateOrUpdate(LibraryId, ParamProc, CallbackParams);
    end,
    Callbacks);
end;

function TLibrariesAccessRoute.AsyncAwaitDelete(const LibraryId: string;
  const ParamProc: TProc<TAccessParams>;
  const Callbacks: TFunc<TPromiseLibrariesAccess>): TPromise<TLibrariesAccess>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TLibrariesAccess>(
    procedure(const CallbackParams: TFunc<TAsyncLibrariesAccess>)
    begin
      AsyncDelete(LibraryId, ParamProc, CallbackParams);
    end,
    Callbacks);
end;

function TLibrariesAccessRoute.AsyncAwaitList(const LibraryId: string;
  const Callbacks: TFunc<TPromiseLibrariesAccessList>): TPromise<TLibrariesAccessList>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TLibrariesAccessList>(
    procedure(const CallbackParams: TFunc<TAsyncLibrariesAccessList>)
    begin
      AsyncList(LibraryId, CallbackParams);
    end,
    Callbacks);
end;

procedure TLibrariesAccessRoute.AsyncCreateOrUpdate(const LibraryId: string;
  const ParamProc: TProc<TAccessParams>;
  const Callbacks: TFunc<TAsyncLibrariesAccess>);
begin
  with TAsyncCallBackExec<TAsyncLibrariesAccess, TLibrariesAccess>.Create(Callbacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TLibrariesAccess
      begin
        Result := Self.CreateOrUpdate(LibraryId, ParamProc);
      end);
  finally
    Free;
  end;
end;

procedure TLibrariesAccessRoute.AsyncDelete(const LibraryId: string;
  const ParamProc: TProc<TAccessParams>;
  const Callbacks: TFunc<TAsyncLibrariesAccess>);
begin
  with TAsyncCallBackExec<TAsyncLibrariesAccess, TLibrariesAccess>.Create(Callbacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TLibrariesAccess
      begin
        Result := Self.Delete(LibraryId, ParamProc);
      end);
  finally
    Free;
  end;
end;

procedure TLibrariesAccessRoute.AsyncList(const LibraryId: string;
  const Callbacks: TFunc<TAsyncLibrariesAccessList>);
begin
  with TAsyncCallBackExec<TAsyncLibrariesAccessList, TLibrariesAccessList>.Create(Callbacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TLibrariesAccessList
      begin
        Result := Self.List(LibraryId);
      end);
  finally
    Free;
  end;
end;

function TLibrariesAccessRoute.CreateOrUpdate(const LibraryId: string;
  const ParamProc: TProc<TAccessParams>): TLibrariesAccess;
begin
  Result := API.Put<TLibrariesAccess, TAccessParams>('libraries/' + LibraryId + '/share', ParamProc);
end;

function TLibrariesAccessRoute.Delete(const LibraryId: string;
  const ParamProc: TProc<TAccessParams>): TLibrariesAccess;
begin
  Result := API.Delete<TLibrariesAccess, TAccessParams>('libraries/' + LibraryId + '/share', ParamProc);
end;

function TLibrariesAccessRoute.List(
  const LibraryId: string): TLibrariesAccessList;
begin
  Result := API.Get<TLibrariesAccessList>('libraries/' + LibraryId + '/share');
end;

end.
