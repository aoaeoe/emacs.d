# 定义 EM 和 EE 变量
EM ?= emacs 
EE ?= $(EM) -Q --batch --eval "(progn (require 'ob-tangle) (setq org-confirm-babel-evaluate nil))"

# 需要加载的目录，可能有lisp site-lisp 等
DS = core etc lang

# 自定义编译模板的函数 tangle_template 用于将 *.org 转为 *.el
define tangle_template
# 检查目录是否存在，不存在就创建
check_dir.$(1):
	@mkdir -p lisp/$(1)

# 目录作为目标，指示新的编译目标
$(1): $(patsubst config/$(1)/%.org, lisp/$(1)/%.el,$(wildcard config/$(1)/*.org))

clean-$(1):
	rm -rf lisp/$(1)

.PHONY: clean-$(1)

# 目标的编译方法
lisp/$(1)/%.el: config/$(1)/%.org
	$(EE) --eval '(org-babel-tangle-publish t "$$<" "$$(@D)/")'
endef

early-init.el: config/early-init.org
	$(EE) --eval '(org-babel-tangle-publish t "$<" "$(@D)/")'

init.el: config/init.org
	$(EE) --eval '(org-babel-tangle-publish t "$<" "$(@D)/")'

# (foreach var, list, template) var 是临时变量，list是需要遍历的列表，template是一个模板字符串
# foreach 遍历 DS 中的每个元素，作为dir传给后面的eval函数求值，所需求值的表达式为call调用模板方法生成编译命令
# 遍历 DS 目录，生成 tangle_template规则
$(foreach dir,$(DS),$(eval $(call tangle_template,$(dir))))

# org-style 编译方法
lisp/org-style/ox-bibtex.el:

el: $(DS) early-init.el init.el

elc:
	$(EM) --batch -l ./init.el -L "lisp" --eval '(byte-recompile-directory "lisp/etc" 0)'
	$(EM) --batch -l ./init.el -L "lisp" --eval '(byte-recompile-directory "lisp/lang" 0)'

generate: el
generate-elc: el elc

clean:
	rm early-init.el init.el
	rm -rf lisp
