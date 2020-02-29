" set background=dark
" set termguicolors
set number
set relativenumber
set so=3
set paste
set noerrorbells
set belloff=all

syntax on
set background=dark
colorscheme solarized

hi Normal ctermbg=NONE guibg=NONE
hi Nontext ctermbg=NONE

" Tab specific option
set tabstop=8                   "A tab is 8 spaces
set expandtab                   "Always uses spaces instead of tabs
set softtabstop=4               "Insert 4 spaces when tab is pressed
set shiftwidth=4                "An indent is 4 spaces
set shiftround                  "Round indent to nearest shiftwidth multiple
set cindent                     "Automatically indent braces
set autoindent                  "Autoindent

noremap <Up> <NOP> 
noremap <Left> <NOP> 
noremap <Down> <NOP> 
noremap <Right> <NOP> 

vmap ,y "+y
map ,p "+p

map <Space><Space> <Esc>/＾▽＾<Enter>"_c3l
" noremap h <NOP>
" noremap l <NOP>

autocmd FileType c map -f i＾▽＾(＾▽＾){<Enter><Tab>＾▽＾<Enter><Backspace>}<Enter>＾▽＾<Esc>4k
autocmd FileType c map -s istruct<Space>＾▽＾{<Enter><Tab>＾▽＾<Enter><Backspace>};<Enter>＾▽＾<Esc>4k
autocmd FileType c map -u iunion<Space>＾▽＾{<Enter><Tab>＾▽＾<Enter><Backspace>};<Enter>＾▽＾<Esc>4k
autocmd FileType c map -c i＾▽＾(＾▽＾){<Enter><Tab>＾▽＾<Enter><Backspace>}<Enter>＾▽＾<Esc>4k

autocmd FileType h map -f i＾▽＾(＾▽＾){<Enter><Tab>＾▽＾<Enter><Backspace>}<Enter>＾▽＾<Esc>4k
autocmd FileType h map -s istruct<Space>＾▽＾{<Enter><Tab>＾▽＾<Enter><Backspace>};<Enter>＾▽＾<Esc>4k
autocmd FileType h map -u iunion<Space>＾▽＾{<Enter><Tab>＾▽＾<Enter><Backspace>};<Enter>＾▽＾<Esc>4k
autocmd FileType h map -c i＾▽＾(＾▽＾){<Enter><Tab>＾▽＾<Enter><Backspace>}<Enter>＾▽＾<Esc>4k
autocmd FileType rmd map <F5> :!echo<Space>"rmarkdown::render('%:p')"<Space>\|<Space>R<Space>--vanilla<Enter>


" Word count function
let g:word_count="<unknown>"

function WordCount()
    return g:word_count
endfunction

function UpdateWordCount()
    let lnum = 1
    let n = 0
    while lnum <= line('$')
        let n = n + len(split(getline(lnum)))
        let lnum = lnum + 1
    endwhile
    let g:word_count = n
endfunction
