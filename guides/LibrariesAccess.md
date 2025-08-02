# Libraries API - Access

- [Introduction](#introduction)
- [List all of the access to this library](#list-all-of-the-access-to-this-library)
- [Create or update an access level](#create-or-update-an-access-level)
- [Delete an access level](#delete-an-access-level)

<br>

___

## Introduction

The ***(beta) Libraries API – Access endpoints*** enable management of access rights for a library. They support three core operations:

- **Listing access:** Retrieve all entities (***users, workspaces, organizations***) that have permissions on a library, along with their access level.

- **Creating or updating access:** Grant or modify an entity’s permission level on a library. Only the library owner can share it or change others’ access. Owners cannot change their own role, and sharing is restricted to within the organization (external sharing is not allowed).

- **Deleting access:** Revoke an entity’s permission on a library. This operation also requires ownership, and an owner cannot remove their own access.

All access management requests require API key authentication. Entities are identified by type (***User, Workspace, Org***) and **UUID**, and when creating or updating access, a permission level such as `Viewer` or `Editor` must be specified.



<br>

___

## List all of the access to this library

Given a library, list all of the Entity that have access and to what level.

Refer to [official documentation](https://docs.mistral.ai/api/#tag/beta.libraries.accesses/operation/libraries_share_list_v1)

```Pascal
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONRequestClear;
  var lib_id := 'lib_id_to_retrieve'; //e.g. 0198502a-6f55-778e-6978-470167da0f28

  //Asynchronous promise example
  var Promise := Client.LibrariesAccess.AsyncAwaitList(lib_id);

  promise
    .&Then<TLibrariesAccessList>(
      function (Value: TLibrariesAccessList): TLibrariesAccessList
      begin
        Result := Value;
        Display(TutorialHub, Value);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);

  //Asynchronous example
//  Client.LibrariesAccess.AsyncList(lib_id,
//    function : TAsyncLibrariesAccessList
//    begin
//      Result.Sender := TutorialHub;
//      Result.OnStart := Start;
//      Result.OnSuccess := Display;
//      Result.OnError := Display;
//    end);

  //Synchronous example
//  var Value := Client.LibrariesAccess.List(lib_id);
//  try
//    Display(TutorialHub, Value);
//  finally
//    Value.Free;
//  end;
```

<br>

___

## Create or update an access level 

Given a library id, you can create or update the access level of an entity. You have to be owner of the library to share a library. An owner cannot change their own role. A library cannot be shared outside of the organization.

Authorizations:
ApiKey

Refer to [official documentation](https://docs.mistral.ai/api/#tag/beta.libraries.accesses/operation/libraries_share_create_v1)

```Pascal
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONRequestClear;
  var lib_id := 'lib_id_to_retrieve'; //e.g. 0198502a-6f55-778e-6978-470167da0f28
  var org_id := 'org_id'; //e.g. 4f36af3c-42f0-437d-a0c2-cdbfe421f348

  //Asynchronous promise example
  var Promise := Client.LibrariesAccess.AsyncAwaitCreateOrUpdate(Edit1.Text,
    procedure (Params: TAccessParams)
    begin
      Params.OrgId(org_id);
      Params.Level(TLevelType.viewer);
      TutorialHub.JSONRequest := Params.ToFormat();
    end);

  promise
    .&Then<TLibrariesAccess>(
      function (Value: TLibrariesAccess): TLibrariesAccess
      begin
        Result := Value;
        Display(TutorialHub, Value);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);

  //Asynchronous example
//  Client.LibrariesAccess.AsyncCreateOrUpdate(Edit1.Text,
//    procedure (Params: TAccessParams)
//    begin
//      Params.OrgId(org_id);
//      Params.Level(TLevelType.viewer);
//      TutorialHub.JSONRequest := Params.ToFormat();
//    end,
//    function : TAsyncLibrariesAccess
//    begin
//      Result.Sender := TutorialHub;
//      Result.OnStart := Start;
//      Result.OnSuccess := Display;
//      Result.OnError := Display;
//    end);

  //Synchronous example
//  var Value := Client.LibrariesAccess.CreateOrUpdate(Edit1.Text,
//    procedure (Params: TAccessParams)
//    begin
//      Params.OrgId(org_id);
//      Params.Level(TLevelType.viewer);
//      TutorialHub.JSONRequest := Params.ToFormat();
//    end);
//  try
//    Display(TutorialHub, Value);
//  finally
//    Value.Free;
//  end;
```

<br>

___

## Delete an access level

Given a library id, you can delete the access level of an entity. An owner cannot delete it's own access. You have to be the owner of the library to delete an acces other than yours.

Refer to [official documentation](https://docs.mistral.ai/api/#tag/beta.libraries.accesses/operation/libraries_share_delete_v1)

```Pascal
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONRequestClear;
  var lib_id := 'lib_id_to_retrieve'; //e.g. 0198502a-6f55-778e-6978-470167da0f28
  var org_id := 'org_id'; //e.g. 4f36af3c-42f0-437d-a0c2-cdbfe421f348

  //Asynchronous promise example
  var Promise := Client.LibrariesAccess.AsyncAwaitDelete(lib_id,
    procedure (Params: TAccessParams)
    begin
      Params.OrgId(org_id);
      TutorialHub.JSONRequest := Params.ToFormat();
    end);

  promise
    .&Then<TLibrariesAccess>(
      function (Value: TLibrariesAccess): TLibrariesAccess
      begin
        Result := Value;
        Display(TutorialHub, Value);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);


  //Asynchronous example
//  Client.LibrariesAccess.AsyncDelete(lib_id,
//    procedure (Params: TAccessParams)
//    begin
//      Params.OrgId(org_id);
//      TutorialHub.JSONRequest := Params.ToFormat();
//    end,
//    function : TAsyncLibrariesAccess
//    begin
//      Result.Sender := TutorialHub;
//      Result.OnStart := Start;
//      Result.OnSuccess := Display;
//      Result.OnError := Display;
//    end);

  //Synchronous example
//  var Value := Client.LibrariesAccess.Delete(lib_id,
//    procedure (Params: TAccessParams)
//    begin
//      Params.OrgId(org_id);
//      TutorialHub.JSONRequest := Params.ToFormat();
//    end);
//  try
//    Display(TutorialHub, Value);
//  finally
//    Value.Free;
//  end;
```


