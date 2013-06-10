new function($) {
  $.fn.setCursorPosition = function(pos) {
    if ($(this).get(0).setSelectionRange) {
      $(this).get(0).setSelectionRange(pos, pos);
    } else if ($(this).get(0).createTextRange) {
      var range = $(this).get(0).createTextRange();
      range.collapse(true);
      range.moveEnd('character', pos);
      range.moveStart('character', pos);
      range.select();
    }
    $(this).focus();
  }
}(jQuery);

(function () {

    var converter1 = Markdown.getSanitizingConverter();

    // tell the converter to use Markdown Extra for tables, fenced_code_gfm, def_list
    Markdown.Extra.init(converter1, {extensions: ["tables", "fenced_code_gfm", "def_list"], highlighter: "prettify"});

    var help = function () {
        var w = window.open(window.location);
        w.isMarkdownHelpPage = true;
    }
    var options = {
        helpButton: { handler: help },
        strings: Markdown.local.zh
    };

    var editor1 = new Markdown.Editor(converter1, null, options);

    var scrollLink = getScrollLink(); 
    scrollLink.onLayoutCreated();
    editor1.hooks.chain("onPreviewRefresh", function () {
        
        // Call onPreviewFinished callbacks when all async preview are finished
        var counter = 0;
        var nbAsyncPreviewCallback = 2; // 1 for waitForImages below and 1 for MathJax below, they are both time consuming task, if only they are both done, begin to caculate md section and scroll bar.
        function tryFinished() {
            if(++counter === nbAsyncPreviewCallback) {
                scrollLink.onPreviewFinished();
            }
        }
        // We assume images are loading in the preview
        $("#wmd-preview").waitForImages(tryFinished);
        // TODO: could we cache the result to speed up ? This action is slow, especially, when there are multiple LaTeX expression on the page, google solution.
        MathJax.Hub.Queue(["Typeset",MathJax.Hub,"wmd-preview"]);
        MathJax.Hub.Queue(tryFinished);

        $('.prettyprint').each(function(){
            $(this).addClass('linenums');
        });
        prettyPrint(); // print code syntax for code snippet if there is.

        $('table').each(function() {
            $(this).addClass('table table-striped table-bordered');
        });

        if (!window.isMarkdownHelpPage) { // Editing on markdown help page won't change local storage
            var preSaveArticle = $('#wmd-input').val();
            var savedArticle = $.localStorage('article');
            if (preSaveArticle != savedArticle) {
                $.localStorage('article', preSaveArticle);
            }
        }
    });
    scrollLink.onEditorConfigure(editor1);

    function popupEditorDialog(title, body, imageClass, placeholder) {
        $('#editorDialog').find('.modal-body input').val("");
        $('#editorDialog').find('.modal-body input').attr("placeholder", placeholder);
        $('#editorDialog').find('#editorDialog-title').text(title);
        $('#editorDialog').find('.modal-body p').text(body);
        $('#editorDialog').find('.modal-body i').removeClass().addClass(imageClass);
        $('#editorDialog').modal({keyboard : true});
    }

    // Custom insert link dialog
    editor1.hooks.set("insertLinkDialog", function(callback) {
        popupEditorDialog('链接', '请输入链接地址', 'icon-link icon-2x', 'http://example.com/ "可选标题"');
        editorDialogCallback = callback;
        return true; // tell the editor that we'll take care of getting the link url
    });

    // Custom insert image dialog
    var editorDialogCallback = null;
    editor1.hooks.set("insertImageDialog", function(callback) {
        popupEditorDialog('图片', '请输入图片地址', 'icon-picture icon-2x', 'http://example.com/images/diagram.jpg "可选标题"');
        editorDialogCallback = callback;
        return true; // tell the editor that we'll take care of getting the image url
    });

    $('#editorDialog').on('hidden', function(){
        if (editorDialogCallback) {
            var url = $('#editorDialog-confirm').data('url');
            if (url) {
                $('#editorDialog-confirm').removeData('url');
                editorDialogCallback(url);
            } else {
                editorDialogCallback(null);
            }
        }
    });

    $('#editorDialog-confirm').click(function(event) {
        var url = $('#editorDialog').find('.modal-body input').val();
        if (url) {
            $(this).data('url', url);
        }
        $('#editorDialog').modal('hide');
    });

    $('#editorDialog').on('shown', function(){
        $('#editorDialog').find('.modal-body input').focus();
    });


    // Make preview if it's inactive in 500ms to reduce the calls in onPreviewRefresh chains above and cpu cost.
    documentContent = undefined;
    var previewWrapper;
    previewWrapper = function(makePreview) {
        var debouncedMakePreview = _.debounce(makePreview, 500);
        return function() {
            if(documentContent === undefined) {
                makePreview();
                documentContent = '';
            } else {
                debouncedMakePreview();
            }
        };
    };

    // To make sure there is no overflow(scroll bar) on the whole page.
    function calculateEditorPreviewHeight() {
        var height = $(window).height() - $('#wmd-preview').position().top - 20;
        $('#wmd-input').height(height);
        $('#wmd-preview').height(height);
    }
    calculateEditorPreviewHeight();
    $(window).resize(function() {
        calculateEditorPreviewHeight();
    });


    // load md help doc from server.
    $.get('static/editor/md-help', function(data) {
        if (data) {
            var article = null;
            var cursorPosition = 0;
            if (window.isMarkdownHelpPage) { // markdown help page is loading the certain text, regardless of local storage.
                article = data;
            } else {
                var article = $.localStorage('article');
                if (!article) {
                    article = data;
                } else {
                    cursorPosition = article.length; // go to the end of the article, if the article is not help doc.
                }
            }
            // Populate editor value
            $('#wmd-input').val(article);
            $('#wmd-input').setCursorPosition(cursorPosition);

            // start editor.
            editor1.run(previewWrapper);

            // Load awesome font to button
            $('#wmd-bold-button > span').addClass('icon-bold muted');
            $('#wmd-italic-button > span').addClass('icon-italic muted');
            $('#wmd-link-button > span').addClass('icon-link muted');
            $('#wmd-quote-button > span').addClass('icon-quote-left muted');
            $('#wmd-code-button > span').addClass('icon-code muted');
            $('#wmd-image-button > span').addClass('icon-picture muted');
            $('#wmd-olist-button > span').addClass('icon-list-ol muted');
            $('#wmd-ulist-button > span').addClass('icon-list-ul muted');
            $('#wmd-heading-button > span').addClass('icon-list-alt muted');
            $('#wmd-hr-button > span').addClass('icon-minus muted');
            $('#wmd-undo-button > span').addClass('icon-undo muted');
            $('#wmd-redo-button > span').addClass('icon-repeat muted');
            $('#wmd-help-button > span').addClass('icon-question-sign muted');


            // create additional new buttons.
            $('#wmd-help-button').before('<li id="wmd-new-button" class="wmd-button" title="新建文件"><span class="icon-file muted"></span></li>');
            $('#wmd-new-button').css('margin-left', '50px');
            $('#wmd-help-button').css('margin-left', '50px');

            $('#wmd-new-button').on('click', function() {
                var answer = confirm('新建文件将会清除当前的文件内容，请确认当前内容已保存');
                if (answer) {
                    $('#wmd-input').val('\n\n\n> *本文使用 [Cmd](http://ghosertblog.github.io/mdeditor/ "中文在线 Markdown 编辑器") 编写*');
                    $('#wmd-input').setCursorPosition(0);
                    editor1.refreshPreview();
                }
            });


            // change color when hovering.
            $('.wmd-button-row').hover(function() {
                $('.wmd-button span').animate({color: '#2C3E50'}, 400);
            },
            function() {
                $('.wmd-button span').animate({color: '#999999'}, 400);
            });

            // enlarge the icon when hovering.
            $('.wmd-button > span').hover(function() {
                $(this).addClass('icon-large');
            },
            function() {
                $(this).removeClass('icon-large');
            });
        }
    });
})();
