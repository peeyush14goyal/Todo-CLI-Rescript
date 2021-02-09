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

let pending_todos_file = "todo.txt"
let completed_todos_file = "done.txt"

if !existsSync(pending_todos_file) {
  writeFileSync(pending_todos_file, "", {encoding: encoding, flag: "w"})
} else {
  ()
}

if !existsSync(completed_todos_file) {
  writeFileSync(completed_todos_file, "", {encoding: encoding, flag: "w"})
} else {
  ()
}

type arg =
  | Add(string)
  | Delete(int)
  | Done(int)
  | Help
  | Report
  | List
  | Empty

let read_file = (filename) => {
   let data = readFileSync(`./${filename}`, {encoding: "utf8", flag: "r"})
  let data_array = Js.String.split("\n", data)
  let data_array = Js.Array.filter((x) => x != "", data_array)
  data_array
}

let append_file = (filename, data) => {
  appendFileSync(`./${filename}`, data , {encoding: "utf8", flag: "a"})
}

let write_file = (filename, data) => {
  writeFileSync(`./${filename}`, data, {encoding: encoding, flag: "w"})
}

let list_of_todos = () => {
 let data_array = read_file(pending_todos_file)

  if Belt.Array.length(data_array) > 0 {
    let array = Js.Array.reverseInPlace(data_array)

    Js.Array.forEachi((todo, index) => {
      let index_val = Belt.Int.toString(Belt.Array.length(array) - index)
      Js.log(`[${index_val}] ${todo}`)
    }, array)
  }
  else{
    Js.log("There are no pending todos!")
  }

}

let add_todos = (todo) => {
  append_file(pending_todos_file, todo ++ eol)
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
  let data_array = read_file(pending_todos_file)
  if index_val > Belt.Array.length(data_array) || index_val < 1 {
    Js.log(`Error: todo #${Belt.Int.toString(index_val)} does not exist.`)
  }
  else {
    let todo = data_array[index_val - 1]
    append_file(completed_todos_file, getToday() ++ " " ++ todo ++ eol)
    let data_array = Js.Array.filteri((_, index) => (index + 1) != index_val, data_array)
    let data_string =Js.Array.joinWith(eol, data_array)
    write_file(pending_todos_file, data_string)
    Js.log(`Marked todo #${Belt.Int.toString(index_val)} as done.`)
  }
}

let delete_todo = (index_val) => {
  let data_array = read_file(pending_todos_file)
  if index_val > Js.Array.length(data_array) || index_val < 1 {
    Js.log(`Error: todo #${Belt.Int.toString(index_val)} does not exist. Nothing deleted.`)
  }
  else {
    let data_array = Js.Array.filteri((_, index) => (index + 1) != index_val, data_array)
    let data_string =Js.Array.joinWith(eol, data_array)
    write_file(pending_todos_file, data_string)
    Js.log(`Deleted todo #${Belt.Int.toString(index_val)}`)
  }
}

let report = () => {
  let pending = read_file(pending_todos_file)
  let completed = read_file(completed_todos_file)
  let pending_count = Belt.Int.toString(Js.Array.length(pending))
  let completed_count = Belt.Int.toString(Js.Array.length(completed))
  Js.log(`${getToday()} Pending : ${pending_count} Completed : ${completed_count}`)
}

let exe_command = command =>
switch command {
| Add(str) => add_todos(str)
| Delete(index) => delete_todo(index)
| List => list_of_todos()
| Done(index) => completed(index)
| Help => help()
| Report => report()
| Empty => ()
}

let value_from_option = x => {
  switch x {
  | None => -1
  | Some(x) => x
  }
}

let get_type = (argument, argv) => {

  if argument == "add" {
    if Js.Array.length(argv) > 3 {
      Add(argv[3])
    }
    else {
      Js.log("Error: Missing todo string. Nothing added!")
      Empty
    }

  }
  else if argument == "del" {
    if Js.Array.length(argv) > 3 {
      let index = argv[3]
      let value = value_from_option(Belt.Int.fromString(index))
      Delete(value)
    }
    else {
      Js.log("Error: Missing NUMBER for deleting todo.")
      Empty
    }
  }
  else if argument == "done" {
    if Js.Array.length(argv) > 3 {
      let index = argv[3]
      let value = value_from_option(Belt.Int.fromString(index))
      Done(value)
    }
    else {
      Js.log("Error: Missing NUMBER for marking todo as done.")
      Empty
    }
  }
  else if argument == "ls" {
    List
  }
  else if argument == "report"{
    Report
  }
  else{
    Help
  }
}

let argv = %raw(`process.argv`)

if Js.Array.length(argv) < 3 {
  Js.log(usage)
}
else {
  let command = argv[2];
  let command_type = get_type(command, argv)
  exe_command(command_type)
}
