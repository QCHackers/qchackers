(function($) {

    $(document).ready(function(){
        $('.js-teleport').click(function(e) {
        $('.js-bobs-code').html('')
        $('.js-bobs-wvf').html('')
        $('.js-alices-wvf').html('')
            e.preventDefault();
            $this = $(this);
            $this.hide()
            $pb = $this.siblings('.progress');
            $pb.show();

            $.post({
                url : '/api/teleportsend',
                data: {'sendit' : 1 },
                success: function(response) {
                    $('.js-alices-wvf').html(response.a);
                    load_progressbar(response);
                },
                dataType: 'json'
            });
        })
    });

    function load_results(response) {
        $('.js-bobs-code').html(response.program);
        $('.js-bobs-wvf').html(response.wvf);
        $('.js-teleport').show()
        $('.progress').hide()
    }

    function load_progressbar(response) {
        var progressBar = $('.js-teleport-progress-bar'),

        width = 0;
        progressBar.width(width);
        var interval = setInterval(function() {
            width += 10;
            progressBar.css('width', width + '%');
            if (width >= 100) {
                clearInterval(interval);
                load_results(response);
            }
        }, 500);

    }

    function sleep(ms) {
        return new Promise(resolve => setTimeout(resolve, ms));
    }

    async function demo() {
        console.log('Taking a break...');
        await sleep(2000);
        console.log('Two second later');
    }
})(jQuery);
