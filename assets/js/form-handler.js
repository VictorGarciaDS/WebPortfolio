function handleSubmit(event) {
    event.preventDefault(); // Evita que el formulario recargue la página
  
    // Captura los datos del formulario
    const form = event.target;
    const formData = new FormData(form);
  
    // Envía los datos a Netlify Forms usando fetch
    fetch("/", {
      method: "POST",
      headers: { "Content-Type": "application/x-www-form-urlencoded" },
      body: new URLSearchParams(formData).toString()
    })
      .then(() => {
        // Muestra el mensaje de agradecimiento
        document.getElementById("thank-you-message").style.display = "block";
        // Limpia los campos del formulario
        form.reset();
      })
      .catch((error) => {
        console.error("Error al enviar el formulario:", error);
        alert("Ocurrió un error al enviar el mensaje. Por favor, inténtalo de nuevo.");
      });
  }
