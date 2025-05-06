$(document).ready(function() {
    $('#bt1').click(function() {
        $('#form1').attr('action',
                       'mailto:victor.garcia@cimat.mx?subject=' +
                       $('#name').val() + '&body=' + $('#consulta').val());
        $('#form1').submit();
    });
});