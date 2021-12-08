$(document).ready(function () {
  const div = document.getElementById("sphere");
  const width = div.clientWidth;
  const height = window.innerHeight;
  const scene = new THREE.Scene();
  const camera = new THREE.PerspectiveCamera(75, width / height, 0.1, 1000);

  //  var ambientLight = new THREE.AmbientLight(0x0000ff, 0.9);
  //  scene.add(ambientLight);
  const lights = [];
  lights[0] = new THREE.PointLight(0xffffff, 1, 0);
  lights[1] = new THREE.PointLight(0xffffff, 1, 0);
  lights[2] = new THREE.PointLight(0xffffff, 1, 0);

  lights[0].position.set(0, 200, 0);
  lights[1].position.set(100, 200, 100);
  lights[2].position.set(-100, -200, -100);

  scene.add(lights[0]);
  scene.add(lights[1]);
  scene.add(lights[2]);

  scene.add(camera);

  // dat.gui controls -------------------------------------------------
  var dgcontrols = new (function () {
    this.rotationSpeed = 0.01;
  })();
  var gui = new dat.GUI({ autoplace: false, width: 300 });
  gui.add(dgcontrols, "rotationSpeed").min(0).max(0.05).name("Rotation speed");

  var object = new THREE.Object3D();
  scene.add(object);

  const renderer = new THREE.WebGLRenderer({ antialias: true });
  renderer.setSize(width, height);
  div.appendChild(renderer.domElement);

  const geomIco = new THREE.IcosahedronGeometry(5, 4);
  //  const geomSphere = new THREE.SphereGeometry(5, 128, 128);
  //  const materialSphere = new THREE.MeshNormalMaterial();
  //var map = new THREE.TextureLoader().load("Rladies2.jpeg");
  //map.wrapS = map.wrapT = THREE.RepeatWrapping;
  //map.anisotropy = 16;
  //map.repeat = new THREE.Vector2(7, 7);
  //map.rotation = Math.PI / 2;
  //var materialSphere = new THREE.MeshPhongMaterial({ map: map });
  //  const meshSphere = new THREE.Mesh(geomSphere, materialSphere);
  //  object.add(meshSphere);
  const matIco = new THREE.MeshPhongMaterial({
    color: 0x156289,
    emissive: 0x072534, //  specular   :  new THREE.Color("rgb(0,0,128)"),
    shininess: 2,
    flatShading: true,
    transparent: 1,
    opacity: 1
  });

  const meshIco = new THREE.Mesh(geomIco, matIco);
  object.add(meshIco);

  const edges = new THREE.EdgesGeometry(geomIco);
  const line = new THREE.LineSegments(
    edges,
    new THREE.LineBasicMaterial({
      color: 0xffffff,
      transparent: false,
      opacity: 1
    })
  );
  object.add(line);

  function sph2cart(rho, theta, phi) {
    return [
      rho * Math.cos(theta) * Math.sin(phi),
      rho * Math.sin(theta) * Math.sin(phi),
      rho * Math.cos(phi)
    ];
  }

  function equator(rho, t) {
    return new THREE.Vector3(
      rho * Math.cos(2 * Math.PI * t),
      rho * Math.sin(2 * Math.PI * t),
      0
    );
  }

  // construction of the key points on the sphere
  const redmat = new THREE.MeshBasicMaterial({ color: 0xff0000 });
  var phi = 1.3;
  var theta = 0;
  for (let i = 0; i < 8; i++) {
    theta += (2 * Math.PI) / 8;
    const keyPoint = sph2cart(5, theta, phi);
    const x = keyPoint[0];
    const y = keyPoint[1];
    const z = keyPoint[2];
    const geom = new THREE.SphereGeometry(0.25).translate(x, y, z);
    const ball = new THREE.Mesh(geom, redmat);
    object.add(ball);
    phi = Math.PI - phi;
  }

  const material = new THREE.MeshBasicMaterial({ color: 0x00ff00 });
  const group = new THREE.Group();
  Shiny.addCustomMessageHandler("spline", function (spline) {
    group.clear();
    for (let i = 0; i < spline.length; i++) {
      const x = spline[i][0];
      const y = spline[i][1];
      const z = spline[i][2];
      const geomball = new THREE.SphereGeometry(0.15).translate(x, y, z);
      const ball = new THREE.Mesh(geomball, material);
      group.add(ball);
    }
    object.add(group);
  });

  camera.position.z = 15;

  const animate = function () {
    requestAnimationFrame(animate);

    object.rotation.x += dgcontrols.rotationSpeed;
    object.rotation.y += dgcontrols.rotationSpeed;

    renderer.render(scene, camera);
  };

  // equator
  const equatorMaterial = new THREE.LineBasicMaterial({
    color: 0x000000,
    linewidth: 3
  });
  const npoints = 100;
  var pts = [];
  for (let i = 0; i <= npoints; i++) {
    pts.push(equator(5, i / npoints));
  }
  const curve = new THREE.CatmullRomCurve3(pts);
  const bgeometry = new THREE.BufferGeometry().setFromPoints(
    curve.getPoints(200)
  );
  const curveObject = new THREE.Line(bgeometry, equatorMaterial);
  object.add(curveObject);

  var isDragging = false;
  var previousMousePosition = { x: 0, y: 0 };

  $(renderer.domElement)
    .on("mousedown", function (e) {
      isDragging = true;
    })
    .on("mousemove", function (e) {
      var deltaMove = {
        x: e.offsetX - previousMousePosition.x,
        y: e.offsetY - previousMousePosition.y
      };
      if (isDragging) {
        var deltaRotationQuaternion = new THREE.Quaternion().setFromEuler(
          new THREE.Euler(
            (Math.PI / 180) * (deltaMove.y * 1),
            (Math.PI / 180) * (deltaMove.x * 1),
            0,
            "XYZ"
          )
        );
        object.quaternion.multiplyQuaternions(
          deltaRotationQuaternion,
          object.quaternion
        );
      }
      previousMousePosition = {
        x: e.offsetX,
        y: e.offsetY
      };
    });

  $(document).on("mouseup", function (e) {
    isDragging = false;
  });

  animate();
});
