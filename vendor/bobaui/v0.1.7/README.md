# BobaUI for Pascal

A TUI (Terminal User Interface) framework for Free Pascal, inspired by the delightful [Bubble Tea](https://github.com/charmbracelet/bubbletea) library for Go.

BobaUI is built on The Elm Architecture (TEA), which provides a clear and robust way to structure applications. It's ideal for building interactive, terminal-based tools and applications.

Here's a glimpse of the example application running in the terminal:
```
┌───────────────────────────┐┌───────────────────────────────────────────────────────────────┐
│ Demos                     ││ Counter Demo                                                  │
│                           ││                                                               │
│ > Counter                 ││ Count: 0                                                      │
│   Name Input              ││ Terminal Size: 80x24                                          │
│                           ││                                                               │
│ Controls:                 ││ SPACE - Increment                                             │
│   ←/→ - Switch Panel      ││ Q - Quit                                                      │
│   ↑/↓ - Select Demo       ││                                                               │
│   ESC - Deselect Input    ││                                                               │
│   Q - Quit                ││                                                               │
└───────────────────────────┘└───────────────────────────────────────────────────────────────┘
```

## The BobaUI Philosophy

BobaUI follows The Elm Architecture, which is composed of three main parts:

*   **Model:** The state of your application. In BobaUI, this is a Pascal class that holds all the data needed to draw your UI.
*   **View:** A way to render your model as a `string`. The view is a pure function of the model; it takes the model's state and returns what should be displayed on the screen.
*   **Update:** A way to update your model based on events. Events can be key presses, terminal resizes, or custom messages you define. The `Update` function takes a message and your current model, and returns a new, updated model and an optional command to execute.

This cycle (Model -> View -> Update -> Model) makes application logic easy to reason about. Since the `Update` function returns a *new* model, state management becomes predictable and less prone to bugs.

## Getting Started

To use BobaUI, you'll need the Free Pascal Compiler (`fpc`).

The core of a BobaUI application is a class that inherits from `bobaui.TModel`. Let's build a simple counter.

### Display Modes

BobaUI supports two display modes:

- **Inline Mode (Default)**: The program renders inline with existing terminal content, preserving scrollback history. This is ideal for command-line tools and utilities.
- **Full Screen Mode**: The program takes over the entire terminal using the alternate screen buffer. This is ideal for full-featured applications.

```pascal
// Inline mode (default)
Prog := TBobaUIProgram.Create(Model);
// or explicitly
Prog := TBobaUIProgram.Create(Model, dmInline);

// Full screen mode
Prog := TBobaUIProgram.Create(Model, dmFullScreen);
```

### A Simple Example

Here is a minimal application that shows a number that you can increment or decrement.

```pascal
program SimpleCounter;

{$mode objfpc}{$H+}

uses
  bobaui, SysUtils;

type
  // Our model holds the application's state.
  TCounterModel = class(bobaui.TModel)
  public
    Count: integer;
    constructor Create;
    function Update(const Msg: bobaui.TMsg): bobaui.TUpdateResult; override;
    function View: string; override;
  end;

constructor TCounterModel.Create;
begin
  inherited Create;
  Count := 0;
end;

// Update handles messages and updates the model.
function TCounterModel.Update(const Msg: bobaui.TMsg): bobaui.TUpdateResult;
var
  NewModel: TCounterModel;
  KeyMsg: bobaui.TKeyMsg;
begin
  Result.Model := Self; // By default, return the same model
  Result.Cmd := nil;

  if Msg is bobaui.TKeyMsg then
  begin
    KeyMsg := bobaui.TKeyMsg(Msg);
    case KeyMsg.Key of
      'q': Result.Cmd := bobaui.QuitCmd; // 'q' quits the program
      ' ': // Space bar increments
        begin
          NewModel := TCounterModel.Create;
          NewModel.Count := Self.Count + 1;
          Result.Model := NewModel; // Return the new model
        end;
    end;
  end;
end;

// View renders the UI as a string.
function TCounterModel.View: string;
begin
  Result := 'Count: ' + IntToStr(Count) + LineEnding + LineEnding +
            '[SPACE] to increment, [Q] to quit.';
end;

var
  Prog: TBobaUIProgram;
  InitialModel: TModel;
begin
  InitialModel := TCounterModel.Create;
  Prog := TBobaUIProgram.Create(InitialModel); // Uses inline mode by default
  Prog.Run;
  Prog.Free;
end.
```

To run this, save it as `counter.pas` and compile with `fpc counter.pas && ./counter`.

## The Core Loop

1.  You create an initial `TModel`.
2.  You pass this model to `TBobaUIProgram.Create()`.
3.  Calling `Prog.Run` starts the BobaUI runtime.
4.  The runtime calls your `View` function to display the initial UI.
5.  It then waits for events (like key presses).
6.  When an event occurs, it's wrapped in a `TMsg` and sent to your `Update` function.
7.  Your `Update` function returns a new model and an optional command.
8.  The runtime replaces the old model with the new one, executes the command, and calls `View` again to render the changes.
9.  This loop continues until a `QuitCmd` is received.

## Messages

Messages are the lifeblood of a BobaUI application. They are triggered by user input or other events and are sent to your `Update` function. The base type is `TMsg`.

BobaUI provides built-in message types:
*   `TKeyMsg`: Represents a key press. You can check `KeyMsg.Key` for simple characters or properties like `IsArrowKey`, `IsUpArrow`, etc.
*   `TWindowSizeMsg`: Sent when the terminal is resized. It contains the new `Width` and `Height`.

You can also define your own custom message types by creating new classes, though this is an advanced use case.

## Commands

Commands (`TCmd`) are for performing actions that have side effects, like quitting the application or making an HTTP request. Your `Update` function can return a command along with the new model.

The most common command is `bobaui.QuitCmd`, which tells the runtime to exit.

## Styling

BobaUI provides a `bobastyle` unit for styling your application. The central piece is the `TStyle` class.

You can use `TStyle` to draw borders, set colors, and define the size of a UI element.

```pascal
uses bobastyle;

var
  MyStyle: TStyle;
  RenderedBox: string;
begin
  MyStyle := TStyle.Create;
  try
    MyStyle.BorderStyle := bsSingle;       // bsNone, bsSingle, bsDouble
    MyStyle.BorderColor := cBrightGreen;    // Various cColor constants
    MyStyle.Width := 40;
    MyStyle.Height := 10;
    MyStyle.Content := 'This is inside a styled box!';
    
    RenderedBox := MyStyle.Render; // Get the final string
  finally
    MyStyle.Free;
  end;
  
  WriteLn(RenderedBox);
end;
```

## Layout

Complex layouts can be created by composing styled elements. The `bobastyle` unit provides helpers for this.

### `JoinHorizontal`

To place two styled elements side-by-side, use `JoinHorizontal`. It takes two rendered strings and a separator. The example application (`main.pas`) uses this to create a sidebar and main content panel.

```pascal
var
  Sidebar, Main, FullUI: string;
  
// Assume Sidebar and Main are strings rendered from TStyle objects...

FullUI := bobastyle.JoinHorizontal(Sidebar, Main, '  ');
```

### `JoinVertical`

To stack elements vertically, use `JoinVertical`. It takes an array of strings and joins them with line endings. This is a convenient way to build vertical layouts without manual string concatenation.

```pascal
var
  Header, Body, Footer, FullUI: string;

Header := 'Header';
Body   := 'Main Content';
Footer := 'Footer';

FullUI := bobastyle.JoinVertical([Header, Body, Footer]);
// Result:
// Header
// Main Content
// Footer
```

## Components

As your application grows, you'll want to organize your code into reusable parts. This is where components come in. A component is essentially a self-contained piece of UI with its own Model, View, and Update logic.

The `bobacomponents` unit provides pre-built components.

### `TInputField`

`TInputField` is a component for text input.

### `TSpinner`

`TSpinner` is an animated loading component similar to those found in Bubble Tea. It provides beautiful Unicode-based spinner animations with self-contained tick-based timing.

#### Spinner Types

BobaUI includes 11 different spinner types:

- `stLine` - Classic line spinner: `| / - \`
- `stDot` - Elegant dot spinner with Braille patterns: `⠋ ⠙ ⠹ ⠸ ⠼ ⠴ ⠦ ⠧ ⠇ ⠏`
- `stMiniDot` - Smaller dot patterns
- `stJump` - Jumping dot animation
- `stPulse` - Pulsing block animation: `█ ▉ ▊ ▋ ▌ ▍ ▎ ▏`
- `stPoints` - Three-dot sequence: `●∙∙ ∙●∙ ∙∙●`
- `stGlobe` - Earth spinning: `🌍 🌎 🌏`
- `stMoon` - Moon phases: `🌑 🌒 🌓 🌔 🌕 🌖 🌗 🌘`
- `stMonkey` - See/hear/speak no evil: `🙈 🙉 🙊`
- `stMeter` - Loading bar: `▁ ▃ ▄ ▅ ▆ ▇ █`
- `stHamburger` - Trigram patterns: `☰ ☱ ☲`

#### Basic Usage

```pascal
uses bobacomponents;

type
  TMyModel = class(TModel)
  private
    FSpinner: TSpinner;
    FSpinning: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function Update(const Msg: TMsg): TUpdateResult; override;
    function View: string; override;
  end;

constructor TMyModel.Create;
begin
  inherited Create;
  FSpinner := TSpinner.Create(stDot); // Create dot spinner
  FSpinning := False;
end;

function TMyModel.Update(const Msg: TMsg): TUpdateResult;
var
  NewSpinner: TSpinner;
  KeyMsg: TKeyMsg;
begin
  Result.Model := Self;
  Result.Cmd := nil;

  if Msg is TKeyMsg then
  begin
    KeyMsg := TKeyMsg(Msg);
    if KeyMsg.Key = ' ' then
    begin
      if FSpinning then
      begin
        FSpinning := False; // Stop spinning
      end
      else
      begin
        FSpinning := True;
        Result.Cmd := FSpinner.Tick; // Start spinning
      end;
    end;
  end
  else if Msg is TComponentTickMsg then
  begin
    if FSpinning then
    begin
      // Update spinner and continue animation
      NewSpinner := FSpinner.Update(Msg);
      if NewSpinner <> FSpinner then
      begin
        FSpinner.Free;
        FSpinner := NewSpinner;
        Result.Cmd := FSpinner.Tick; // Schedule next frame
      end;
    end;
  end;
end;

function TMyModel.View: string;
begin
  if FSpinning then
    Result := 'Loading... ' + FSpinner.View
  else
    Result := 'Press SPACE to start spinner';
end;
```

#### Key Features

- **Self-Contained Animation**: Each spinner manages its own timing using `ComponentTickCmd`
- **Multiple Types**: 11 different built-in spinner animations
- **Component Isolation**: Multiple spinners can run simultaneously without interference
- **Styling Support**: Integration with BobaUI's styling system
- **Memory Safe**: Proper cleanup and immutable updates

### `TList`

`TList` is a component for creating selectable lists with keyboard navigation. It provides:

- Scrollable list display with automatic overflow handling
- Customizable selection indicator (default: `> `)
- Callback mechanism for handling item selection
- Optional border and title
- Vi-style navigation keys (j/k) in addition to arrow keys

#### Basic Usage

```pascal
uses bobacomponents;

type
  TMyModel = class(TModel)
  private
    FList: TList;
    procedure OnItemSelect(Index: integer; const Item: string);
  public
    constructor Create;
    destructor Destroy; override;
    function Update(const Msg: TMsg): TUpdateResult; override;
    function View: string; override;
  end;

constructor TMyModel.Create;
begin
  inherited Create;
  FList := TList.Create;
  FList.Title := 'Choose an Option';
  FList.Width := 30;
  FList.Height := 10;
  FList.OnSelect := @OnItemSelect;
  
  // Add items
  FList.AddItem('Option 1');
  FList.AddItem('Option 2');
  FList.AddItem('Option 3');
end;

procedure TMyModel.OnItemSelect(Index: integer; const Item: string);
begin
  WriteLn('Selected: ', Item, ' at index ', Index);
end;

function TMyModel.Update(const Msg: TMsg): TUpdateResult;
var
  NewList: TList;
begin
  Result.Model := Self;
  Result.Cmd := nil;
  
  // Delegate to list for keyboard handling
  NewList := FList.Update(Msg);
  if NewList <> FList then
  begin
    // List state changed, update model
    // ... create new model instance and update list
  end;
end;

function TMyModel.View: string;
begin
  Result := FList.View;
end;
```

#### List Properties

- `Items: TStringArray` - The array of items in the list
- `SelectedIndex: integer` - Currently selected item index
- `Width, Height: integer` - Dimensions of the list
- `Title: string` - Optional title displayed above the list
- `SelectionIndicator: string` - Indicator for selected item (default: `> `)
- `ShowBorder: boolean` - Whether to show a border (default: true)
- `BorderStyle: TBorderStyle` - Style of the border (single, double, etc.)
- `BorderColor: TColor` - Color of the border
- `SelectedColor: TColor` - Color of the selected item text
- `OnSelect: TListSelectCallback` - Callback when Enter is pressed

#### Navigation Keys

- **↑/↓** or **j/k** - Move selection up/down
- **g** - Jump to first item
- **G** - Jump to last item
- **Enter** - Select current item (triggers OnSelect callback)

See `examples/list_demo.pas` for a complete example showing a seasonal list with custom selection handling.

#### Component Pattern

Components like `TInputField`, `TSpinner`, and `TList` do not inherit from `TModel` directly, but are managed by a model. Your application's model will hold an instance of the component, delegate messages to it, and call its `View` function.

Let's look at the pattern from `main.pas`:

1.  **Model:** Your main model holds an instance of the component (e.g., `FInputField: TInputField`).
2.  **View:** In your main `View` function, you call the component's `View` function and place the resulting string in your layout. `MyInputField.View`.
3.  **Update:** In your main `Update` function, if a message is relevant to the component, you delegate it. The component's `Update` function will return a *new instance* of the component with its state updated. You then replace the old component instance in your model with the new one.

```pascal
// In your model's Update function
if (Msg is TKeyMsg) and InputIsFocused then
begin
  NewInputField := FInputField.Update(Msg);
  if NewInputField <> FInputField then
  begin
    // The input field's state changed.
    // Create a new model, and replace the old input field with the new one.
    NewModel := TMainModel.Create;
    ... // copy other state from Self to NewModel
    NewModel.FInputField.Free; // Free the old one
    NewModel.FInputField := NewInputField; // Assign the new one
    Result.Model := NewModel;
  end;
end;
```

This pattern keeps the component logic encapsulated while allowing the main application to control when and where the component is active.

## Animations

BobaUI supports component-specific tick-based animations, inspired by Bubble Tea. This approach provides granular control, better performance, and allows components to manage their own animation lifecycle.

### Component-Specific Tick-Based Animations

```pascal
function TSpinnerModel.Update(const Msg: TMsg): TUpdateResult;
var
  ComponentTickMsg: TComponentTickMsg;
begin
  Result.Model := Self;
  Result.Cmd := nil;

  if Msg is TComponentTickMsg then
  begin
    ComponentTickMsg := TComponentTickMsg(Msg);
    
    // Only handle ticks for this component
    if ComponentTickMsg.ComponentId = 'my-spinner' then
    begin
      // Update component state
      Result.Model := UpdatedModel;
      // Schedule next tick to continue animation
      Result.Cmd := ComponentTickCmd('my-spinner', 100); // 100ms interval
    end;
  end;
end;
```

#### Advantages of Tick-Based Animation

- **Granular Control**: Each component manages its own animation timing
- **Better Performance**: Only active components animate
- **Flexible Timing**: Different components can have different frame rates
- **Self-Contained**: No global animation state to manage
- **Component Isolation**: Multiple animated components don't interfere with each other
- **Resource Efficient**: Components only consume CPU when actually animating

To start a tick-based animation, return a `ComponentTickCmd` from your Update function:

```pascal
// Start animation with 200ms intervals
Result.Cmd := ComponentTickCmd('my-component', 200);
```

### Animation Best Practices

1. **Performance**: Only start animations when needed. Stop them when your animated content is not visible to save CPU resources.

2. **Timing Consistency**: Use consistent tick intervals for smooth animations:
   ```pascal
   // Good: Consistent timing
   Result.Cmd := ComponentTickCmd('my-component', 33); // ~30 FPS
   
   // Consider your needs: faster for smooth, slower for efficiency
   Result.Cmd := ComponentTickCmd('spinner', 100);     // 10 FPS for simple spinner
   ```

3. **State Management**: Keep animation state (like time, position, etc.) in your model and create new model instances when the state changes.

4. **Input Priority**: The animation system is designed to prioritize user input over animation messages, ensuring responsive controls even during heavy animation.

### Example: Animated Gradient

Here's a simplified example of how the gradient demo implements tick-based animation:

```pascal
type
  TGradientModel = class(TModel)
  private
    FTime: Double;
    FAnimationSpeed: Double;
    FAnimating: Boolean;
  public
    constructor Create;
    function Update(const Msg: TMsg): TUpdateResult; override;
    function View: string; override;
  end;

function TGradientModel.Update(const Msg: TMsg): TUpdateResult;
var
  NewModel: TGradientModel;
  ComponentTickMsg: TComponentTickMsg;
begin
  Result.Model := Self;
  Result.Cmd := nil;

  if Msg is TComponentTickMsg then
  begin
    ComponentTickMsg := TComponentTickMsg(Msg);
    
    // Only handle our component's ticks
    if (ComponentTickMsg.ComponentId = 'gradient') and FAnimating then
    begin
      // Create new model with updated time
      NewModel := TGradientModel.Create;
      NewModel.FTime := FTime + (0.033 * FAnimationSpeed); // 33ms tick
      NewModel.FAnimationSpeed := FAnimationSpeed;
      NewModel.FAnimating := FAnimating;
      Result.Model := NewModel;
      
      // Schedule next tick to continue animation
      Result.Cmd := ComponentTickCmd('gradient', 33);
    end;
  end;
end;

function TGradientModel.View: string;
var
  HueShift: Double;
begin
  // Use time for animation calculations
  HueShift := FTime * 60.0; // 60 degrees per second
  
  // Create animated content based on current time
  Result := CreateAnimatedContent(HueShift);
end;
```

The animation system automatically handles frame timing, message prioritization, and resource management, allowing you to focus on creating engaging animated experiences.

## Overlays

BobaUI supports overlays for creating modal dialogs, menus, tooltips, and other UI elements that appear on top of existing content. The overlay system uses a z-index approach for layering and integrates seamlessly with the existing Model-View-Update architecture.

### Core Concepts

**Z-Index Layering**: Overlays are positioned using integer z-index values. Higher numbers render on top of lower numbers.

**Absolute Positioning**: Overlays are positioned using screen coordinates with the `TRect` type:
```pascal
type
  TRect = record
    X, Y: Integer;        // Top-left position (1-based coordinates)
    Width, Height: Integer;  // Dimensions
  end;
```

**Event Priority**: Overlays with higher z-index values receive events first, allowing proper modal behavior.

### Basic Usage

#### Creating an Overlay

```pascal
// Create the overlay content (any TModel)
DialogModel := TConfirmDialogModel.Create('Title', 'Message');

// Position the overlay (centered on screen)
Position := CenterRectInBounds(50, 10, ScreenWidth, ScreenHeight);

// Add the overlay to your model
AddOverlay(TOverlay.Create(DialogModel, Position, 1000)); // z-index 1000
```

#### Handling Overlay Events

In your model's `Update` method, handle overlay events first:

```pascal
function TMainModel.Update(const Msg: TMsg): TUpdateResult;
begin
  Result.Model := Self;
  Result.Cmd := nil;

  // First try overlay handling
  Result := UpdateOverlays(Msg);
  if Result.Model <> Self then
    Exit; // Overlay handled the message
  
  // Handle base application events
  if Msg is TKeyMsg then
  begin
    // Your main application logic here
  end;
end;
```

#### Overlay Management

```pascal
// Add an overlay
AddOverlay(TOverlay.Create(ContentModel, Position, ZIndex));

// Remove the top overlay
RemoveOverlay(Length(FOverlays) - 1);

// Clear all overlays
ClearOverlays;

// Check if overlays exist
if HasOverlays then
  // Handle overlay-specific logic
```

### Positioning Functions

BobaUI provides helper functions for common positioning patterns:

```pascal
// Center an overlay on screen
Position := CenterRectInBounds(Width, Height, ScreenWidth, ScreenHeight);

// Create custom position
Position := CreateRect(X, Y, Width, Height);

// Simple centering (uses default 80x24 dimensions)
Position := CenterRect(Width, Height);
```

### Z-Index Conventions

Recommended z-index ranges for different overlay types:

- **0-99**: Base content layers
- **100-499**: Tooltips and hover effects  
- **500-899**: Dropdown menus and popups
- **900-999**: Modal dialogs and overlays
- **1000+**: System overlays and notifications

### Complete Example: Modal Dialog

Here's a complete example of creating a modal confirmation dialog:

```pascal
type
  TConfirmDialogModel = class(TModel)
  private
    FTitle, FMessage: string;
    FSelectedOption: Integer; // 0 = Yes, 1 = No
  public
    constructor Create(const ATitle, AMessage: string);
    function View: string; override;
    function Update(const Msg: TMsg): TUpdateResult; override;
    property SelectedOption: Integer read FSelectedOption;
  end;

constructor TConfirmDialogModel.Create(const ATitle, AMessage: string);
begin
  inherited Create;
  FTitle := ATitle;
  FMessage := AMessage;
  FSelectedOption := 0; // Default to "Yes"
end;

function TConfirmDialogModel.View: string;
var
  Style: TStyle;
  Content: string;
  YesButton, NoButton: string;
begin
  // Create button text based on selection
  if FSelectedOption = 0 then
  begin
    YesButton := '> [Yes]';
    NoButton := '  [No] ';
  end
  else
  begin
    YesButton := '  [Yes]';
    NoButton := '> [No] ';
  end;
  
  Content := JoinVertical([
    FMessage,
    '',
    YesButton + '   ' + NoButton,
    '',
    'Use ←/→ to select, Enter to confirm, Esc to cancel'
  ]);
  
  Style := TStyle.Create;
  try
    Style.BorderStyle := bsDouble;
    Style.BorderColor := cBrightRed;
    Style.Width := 52;
    Style.Height := 10;
    Style.Content := Content;
    Style.Title := FTitle;
    Style.TitlePosition := tpCenter;
    Style.TitleColor := cBrightWhite;
    
    Result := Style.Render;
  finally
    Style.Free;
  end;
end;

function TConfirmDialogModel.Update(const Msg: TMsg): TUpdateResult;
var
  NewModel: TConfirmDialogModel;
  KeyMsg: TKeyMsg;
begin
  Result.Model := Self;
  Result.Cmd := nil;

  if Msg is TKeyMsg then
  begin
    KeyMsg := TKeyMsg(Msg);
    
    if KeyMsg.IsLeftArrow and (FSelectedOption > 0) then
    begin
      NewModel := TConfirmDialogModel.Create(FTitle, FMessage);
      NewModel.FSelectedOption := FSelectedOption - 1;
      Result.Model := NewModel;
    end
    else if KeyMsg.IsRightArrow and (FSelectedOption < 1) then
    begin
      NewModel := TConfirmDialogModel.Create(FTitle, FMessage);
      NewModel.FSelectedOption := FSelectedOption + 1;
      Result.Model := NewModel;
    end;
    // Enter and Escape handling would be done by parent model
  end;
end;

// In your main model:
function TMainModel.Update(const Msg: TMsg): TUpdateResult;
var
  DialogModel: TConfirmDialogModel;
  DialogPosition: TRect;
begin
  // Handle overlay events first
  Result := UpdateOverlays(Msg);
  if Result.Model <> Self then
    Exit;
    
  if Msg is TKeyMsg then
  begin
    KeyMsg := TKeyMsg(Msg);
    
    if KeyMsg.Key = ' ' then // Space - show dialog
    begin
      DialogModel := TConfirmDialogModel.Create('Confirmation', 'Are you sure?');
      DialogPosition := CenterRectInBounds(52, 10, FTerminalWidth, FTerminalHeight);
      AddOverlay(TOverlay.Create(DialogModel, DialogPosition, 1000));
      Result.Model := Self;
    end
    else if KeyMsg.Key = #27 then // Escape - close dialog
    begin
      ClearOverlays;
      Result.Model := Self;
    end;
  end;
end;
```

### Best Practices

1. **Event Handling**: Always call `UpdateOverlays()` first in your Update method to ensure proper event routing.

2. **Memory Management**: Overlays are automatically cleaned up when removed or when the parent model is destroyed.

3. **Visual Design**: Use contrasting colors and clear borders to make overlays visually distinct from base content.

4. **Positioning**: Ensure overlays fit within screen bounds and handle window resize events appropriately.

5. **Z-Index Organization**: Use consistent z-index ranges to maintain predictable layering behavior.

6. **Modal Behavior**: For modal dialogs, handle escape key and background clicks to provide intuitive close behavior.

### Performance

The overlay system is designed for minimal performance impact:

- **Zero overhead** when no overlays are present
- **Efficient compositing** that only updates changed screen areas
- **Differential rendering** integration for optimal screen updates
- **Memory-efficient** with automatic cleanup

See `examples/modal_demo.pas` for a complete working example of the overlay system in action.

## Running the Demos

BobaUI includes several example applications that demonstrate different features and patterns. All examples are located in the `examples/` directory.

### Building and Running Examples

BobaUI uses a Makefile for building examples. To get started:

```bash
# Build all examples
make

# Build a specific example
make layout_example

# Clean build artifacts
make clean

# View all available targets
make help
```

Examples are compiled to the `bin/` directory. You can run them directly:

```bash
# Run the layout example
./bin/layout_example

# Run the spinner demo
./bin/auto_spinner_demo

# Run with version flag (if supported)
./bin/layout_example --version
```

### Available Examples

- **`layout_example`** - Demonstrates the sidebar/main panel layout with navigation and input handling (full screen)
- **`auto_spinner_demo`** - Shows various spinner animations with automatic cycling (inline mode)
- **`background_test`** - Tests background color and RGB color capabilities (terminal output only)
- **`gradient_demo`** - Animated rainbow gradient text with smooth color transitions (full screen)
- **`menu_demo`** - DOS-style menu system with keyboard navigation (full screen)
- **`modal_demo`** - Modal dialog overlay system with confirmation dialogs (full screen)
- **`list_demo`** - Interactive list component with seasonal themes and selection callbacks (full screen)

Each example showcases different aspects of the BobaUI framework and can serve as starting points for your own applications. Note that most examples use full screen mode for better visual impact, while the spinner demo demonstrates inline mode.

## Contributing

We welcome contributions to BobaUI! Please see [CONTRIBUTING.md](CONTRIBUTING.md) for detailed information about:

- Setting up your development environment
- Running tests and writing new tests
- Code style guidelines and architecture patterns
- Submitting pull requests and contribution workflow

Whether you're fixing a bug, adding a feature, or creating examples, your contributions help make BobaUI better for everyone!