local api = vim.api

local lfefileloaded = function(ev)
  api.nvim_set_option_value("filetype", "lfe", {buf = 0})
end

api.nvim_create_autocmd({"BufRead", "BufNewFile"}, {
  pattern = "*.lfe",
  callback = lfefileloaded,
})

local lfegroupid = api.nvim_create_augroup("LFE", {
  clear = false
})

api.nvim_create_autocmd({"BufRead", "BufNewFile"}, {
  pattern = {"*.lfe"},
  group = lfegroupid,
  callback = function(ev)
    vim.api.nvim_buf_set_option(0, "commentstring", "; %s")
  end})

