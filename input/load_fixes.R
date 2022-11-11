fixes <- list(
  dos_hearings = function(entry){
    entry$document <- entry$document %>%
      mutate(person = str_replace(person, "^State Sen\\.", "Senator")) %>%
      mutate(person = str_replace(person, "^Sen\\.", "Senator"))
    entry
  },
  congress = function(entry){
    entry
  },
  other = function(entry){
    entry
  },
  ne_legislature = function(entry){
    entry$document <- entry$document %>%
      mutate(content = str_remove_all(content, "\\[[^\\]]*\\]"))
    entry
  },
  letters = function(entry){
    entry
  }
)
