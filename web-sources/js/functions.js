
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