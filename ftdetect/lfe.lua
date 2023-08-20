local api = vim.api

local lfefileloaded = function(e)
  api.nvim_set_option_value("filetype", "lfe", {buf = 0})
end

api.nvim_create_autocmd({"BufRead", "BufNewFile"}, {
  pattern = "*.lfe",
  callback = lfefileloaded,
})

