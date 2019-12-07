
function hover(element) {
    var l_index =  element.getAttribute("src").lastIndexOf("/");
    var route =  element.getAttribute("src").substring(0, l_index+1);
    var archive_name =  element.getAttribute("src").substring(l_index+1);
    var ns = route.concat("hover").concat(archive_name);
    element.setAttribute('src', ns);
}

function unhover(element) {
    var e = element.getAttribute("src").replace("hover", "");
    element.setAttribute('src', e);
}

function openPopUp(element){
    element.style.display = "inline-block";
}

function closePopUp(element){
    element.style.display = "none";
}

function download(filename, text){

    var element = document.createElement('a');
    element.setAttribute('href', 'data:text/plain;charset=utf-8,' + encodeURIComponent(text));
    element.setAttribute('download', filename);
    element.style.display = 'none';
    document.body.appendChild(element);
    element.click();
    document.body.removeChild(element);
}
        
function plotDot(text){
    d3.select("#graph").graphviz()
        .fade(false)
        .renderDot(text);
}