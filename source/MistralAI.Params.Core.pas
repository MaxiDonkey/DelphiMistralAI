unit MistralAI.Params.Core;

interface

uses
  System.SysUtils, System.Classes;

type
  /// <summary>
  /// Generic interface for managing parameters of type <c>T</c>.
  /// </summary>
  /// <typeparam name="T">
  /// The type of the parameters.
  /// </typeparam>
  IUseParams<T> = interface
    ['{18566F2C-F2D9-4257-A460-D9AE8F053357}']

    /// <summary>
    /// Sets the parameters.
    /// </summary>
    /// <param name="Value">
    /// The value of the parameters to be set.
    /// </param>
    procedure SetParams(const Value: T);

    /// <summary>
    /// Gets the current parameters.
    /// </summary>
    /// <returns>
    /// The current parameters of type <c>T</c>.
    /// </returns>
    function GetParams: T;

    /// <summary>
    /// Assigns the parameters using a function.
    /// </summary>
    /// <param name="Value">
    /// A function that returns parameters of type <c>T</c>.
    /// </param>
    procedure Assign(Value: TFunc<T>);

    /// <summary>
    /// Returns the current instance as an object of type <c>TObject</c>.
    /// </summary>
    /// <returns>
    /// The instance cast to <c>TObject</c>.
    /// </returns>
    function AsSender: TObject;

    /// <summary>
    /// Provides access to the parameters as a property.
    /// </summary>
    property Params: T read GetParams write SetParams;
  end;

  /// <summary>
  /// A factory class for creating instances of <c>IUseParams</c>.
  /// </summary>
  /// <param name="T">
  /// The type of the parameters for which the instance is created.
  /// </param>
  TUseParamsFactory<T> = class

    /// <summary>
    /// Creates and returns a new instance of <c>IUseParams</c> for the specified type <c>T</c>.
    /// </summary>
    /// <returns>
    /// A new instance of <c>IUseParams&lt;T&gt;</c>.
    /// </returns>
    class function CreateInstance: IUseParams<T>;
  end;

  /// <summary>
  /// A generic class implementing the <c>IUseParams</c> interface to manage parameters of type <c>T</c>.
  /// </summary>
  /// <param name="T">
  /// The type of the parameters.
  /// </param>
  TUseParams<T> = class(TInterfacedObject, IUseParams<T>)
  private
    FParams: T;

    /// <summary>
    /// Sets the parameters to the provided value.
    /// </summary>
    /// <param name="Value">
    /// The new parameters value.
    /// </param>
    procedure SetParams(const Value: T);

    /// <summary>
    /// Retrieves the current parameters value.
    /// </summary>
    /// <returns>
    /// The current parameters.
    /// </returns>
    function GetParams: T;

  protected
    /// <summary>
    /// Casts the instance as a <c>TObject</c> for use as the sender of events.
    /// </summary>
    /// <returns>
    /// The current instance cast to <c>TObject</c>.
    /// </returns>
    function AsSender: TObject;

    /// <summary>
    /// Assigns the parameters using a function that returns type <c>T</c>.
    /// </summary>
    /// <param name="Value">
    /// A function that sets the parameters.
    /// </param>
    procedure Assign(Value: TFunc<T>);

  public
    /// <summary>
    /// Property to get or set the parameters.
    /// </summary>
    property Params: T read GetParams write SetParams;
  end;

implementation

{ TUseParams<T> }

function TUseParams<T>.AsSender: TObject;
begin
  Result := Self;
end;

procedure TUseParams<T>.Assign(Value: TFunc<T>);
begin
  if Assigned(Value) then
    begin
      var X := Value;
      Params := X;
    end;
end;

function TUseParams<T>.GetParams: T;
begin
  Result := FParams;
end;

procedure TUseParams<T>.SetParams(const Value: T);
begin
  FParams := Value;
end;

{ TUseParamsFactory<T> }

class function TUseParamsFactory<T>.CreateInstance: IUseParams<T>;
begin
  Result := TUseParams<T>.Create;
end;

end.
