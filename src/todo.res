/*
Sample JS implementation of Todo CLI that you can attempt to port:
https://gist.github.com/jasim/99c7b54431c64c0502cfe6f677512a87
*/

/* Returns date with the format: 2021-02-04 */
let getToday: unit => string = %raw(`
function() {
  let date = new Date();
  return new Date(date.getTime() - (date.getTimezoneOffset() * 60000))
    .toISOString()
    .split("T")[0];
}
  `)

type fsConfig = {encoding: string, flag: string}

/* https://nodejs.org/api/fs.html#fs_fs_existssync_path */
@bs.module("fs") external existsSync: string => bool = "existsSync"

/* https://nodejs.org/api/fs.html#fs_fs_readfilesync_path_options */
@bs.module("fs")
external readFileSync: (string, fsConfig) => string = "readFileSync"

/* https://nodejs.org/api/fs.html#fs_fs_writefilesync_file_data_options */
@bs.module("fs")
external appendFileSync: (string, string, fsConfig) => unit = "appendFileSync"

@bs.module("fs")
external writeFileSync: (string, string, fsConfig) => unit = "writeFileSync"

/* https://nodejs.org/api/os.html#os_os_eol */
@bs.module("os") external eol: string = "EOL"

let encoding = "utf8"

/*
NOTE: The code below is provided just to show you how to use the
date and file functions defined above. Remove it to begin your implementation.
*/

let pendingTodosFile = "todo.txt"
let completedTodosFile = "done.txt"

if !existsSync(pendingTodosFile) {
  writeFileSync(pendingTodosFile, "", {encoding: encoding, flag: "w"})
}

if !existsSync(completedTodosFile) {
  writeFileSync(completedTodosFile, "", {encoding: encoding, flag: "w"})
}

type arg =
  | Add(option<string>)
  | Delete(option<int>)
  | Done(option<int>)
  | Help
  | Report
  | List

let readFile = (filename) => {

  if (!existsSync(filename)) {
     []
  }
  else {
   let text = readFileSync(filename, {encoding: "utf8", flag: "r"})
  let lines = Js.String.split(eol, text)
  let lines = Js.Array.filter(text => Js.String.length(text) > 0, lines)
  lines
  }
}

let appendFile = (filename, text) => {
  appendFileSync(filename, text , {encoding: "utf8", flag: "a"})
}

let writeFile = (filename, lines) => {
  let text = Js.Array.joinWith(eol, lines)
  writeFileSync(filename, text, {encoding: encoding, flag: "w"})
}


let listOfTodos = () => {
 let data = readFile(pendingTodosFile)

  if Belt.Array.length(data) > 0 {
    let array = Js.Array.reverseInPlace(data)

    Js.Array.forEachi((todo, index) => {
      let index_val = Belt.Int.toString(Belt.Array.length(array) - index)
      Js.log(`[${index_val}] ${todo}`)
    }, array)
  }
  else{
    Js.log("There are no pending todos!")
  }

}

let addTodo = (todo) => {
  appendFile(pendingTodosFile, todo ++ eol)
  Js.log(`Added todo: "${todo}"`)
}

let usage = `Usage :-
$ ./todo add "todo item"  # Add a new todo
$ ./todo ls               # Show remaining todos
$ ./todo del NUMBER       # Delete a todo
$ ./todo done NUMBER      # Complete a todo
$ ./todo help             # Show usage
$ ./todo report           # Statistics`

let help = () => {
  Js.log(usage)
}

let completed = (index_val) => {
  let data = readFile(pendingTodosFile)

  if index_val > Belt.Array.length(data) || index_val < 1 {
    Js.log(`Error: todo #${Belt.Int.toString(index_val)} does not exist.`)
  }
  else {
    let todo = data[index_val - 1]
    appendFile(completedTodosFile, todo ++ eol)

    let _ = Js.Array.spliceInPlace(~pos = index_val-1,~remove = 1,~add = [], data)
    writeFile(pendingTodosFile, data)
    Js.log(`Marked todo #${Belt.Int.toString(index_val)} as done.`)
  }
}

let deleteTodo = (index_val) => {
  let data = readFile(pendingTodosFile)

  if index_val > Js.Array.length(data) || index_val < 1 {
    Js.log(`Error: todo #${Belt.Int.toString(index_val)} does not exist. Nothing deleted.`)
  }
  else {
    let _ = Js.Array.spliceInPlace(~pos = index_val-1,~remove = 1,~add = [], data)
    writeFile(pendingTodosFile, data)
    Js.log(`Deleted todo #${Belt.Int.toString(index_val)}`)
  }
}

let report = () => {
  let pending = readFile(pendingTodosFile)
  let completed = readFile(completedTodosFile)
  let pending_count = Belt.Int.toString(Js.Array.length(pending))
  let completed_count = Belt.Int.toString(Js.Array.length(completed))
  Js.log(`${getToday()} Pending : ${pending_count} Completed : ${completed_count}`)
}


let exeCommand = command =>
switch command {
| Add(str) => {
    switch str {
      | Some(x) => addTodo(x)
      | None => Js.log("Error: Missing todo string. Nothing added!")
    }
  }
| Delete(index) => {
    switch index {
      | Some(x) => deleteTodo(x)
      | None => Js.log("Error: Missing NUMBER for deleting todo.")
    }
  }
| List => listOfTodos()
| Done(index) => {
    switch index {
      | Some(x) => completed(x)
      | None => Js.log("Error: Missing NUMBER for marking todo as done.")
    }
  }
| Help => help()
| Report => report()
}



let getType = (argument, argv) => {

  switch argument {
  | "add" => {
      let value = argv->Belt.Array.get(3)
      Add(value)
    }
  | "del" => {
      let value = argv->Belt.Array.get(3)->Belt.Option.flatMap(Belt.Int.fromString)
      Delete(value)
    }

  | "done" => {
      let value = argv->Belt.Array.get(3)->Belt.Option.flatMap(Belt.Int.fromString)
      Done(value)
  }
  | "ls" => List
  | "report" => Report
  | _ => Help
  }
}

@val @scope("process") external argv: array<string> = "argv"

if Js.Array.length(argv) < 3 {
  Js.log(usage)
}
else {
  let command = argv[2];
  let command_type = getType(command, argv)
  exeCommand(command_type)
}
